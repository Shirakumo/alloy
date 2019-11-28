#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.renderers.opengl.msdf
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl)
   (#:font-discovery #:org.shirakumo.font-discovery)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors))
  (:export))
(in-package #:org.shirakumo.alloy.renderers.opengl.msdf)

(defclass renderer (opengl:renderer)
  ())

(defmethod alloy:allocate :before ((renderer renderer))
  (setf (opengl:resource 'text-vbo renderer)
        (opengl:make-vertex-buffer renderer (make-array 0 :element-type 'single-float)
                                   :data-usage :dynamic-draw))
  (setf (opengl:resource 'text-vao renderer)
        (opengl:make-vertex-array renderer `((,(opengl:resource 'text-vbo renderer) :size 2 :stride 16 :offset 0)
                                             (,(opengl:resource 'text-vbo renderer) :size 2 :stride 16 :offset 8))))
  (setf (opengl:resource 'text-shader renderer)
        (opengl:make-shader renderer :vertex-shader "#version 330 core
layout (location=0) in vec2 pos;
layout (location=1) in vec2 in_uv;
uniform mat3 transform;
out vec2 uv;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1), 1);
  uv = in_uv;
}"
                                     :fragment-shader "#version 330 core
in vec2 uv;
uniform sampler2D image;
uniform float pxRange = 4;
uniform vec4 color = vec4(0, 0, 0, 1);
out vec4 out_color;

float median(float r, float g, float b) {
    return max(min(r, g), min(max(r, g), b));
}

void main() {
    vec2 msdfUnit = pxRange/vec2(textureSize(image, 0));
    vec3 msdfData = texture(image, uv).rgb;
    float sigDist = median(msdfData.r, msdfData.g, msdfData.b) - 0.5;
    sigDist *= dot(msdfUnit, 0.5/fwidth(uv));
    float opacity = clamp(sigDist + 0.5, 0.0, 1.0);
    out_color = opacity*color;
}")))

(defclass font (simple:font)
  ((file :initarg :file :accessor file)
   (data :accessor data)
   (atlas :accessor atlas)))

(defmethod alloy:allocate ((font font))
  (unless (slot-boundp font 'data)
    ;; FIXME: Check if file is cached msdf font, or ttf/otf, and cache in latter case.
    (let* ((data (3b-bmfont:read-bmfont (file font)))
           (file (merge-pathnames (getf (aref (3b-bmfont:pages data) 0) :file) (file font)))
           (pixels (load-image file)))
      (setf (data font) data)
      (opengl:make-texture renderer widht height pixels))))

(defmethod simple:request-font ((renderer renderer) (family string) &key slant spacing weight stretch)
  (let ((font (font-discovery:find-font
               :family family
               :weight weight
               :stretch stretch
               :spacing spacing
               :slant slant)))
    (make-instance 'font :file (font-discovery:file font)
                         :family (font-discovery:family font)
                         :weight (font-discovery:weight font)
                         :stretch (font-discovery:stretch font)
                         :spacing (font-discovery:spacing font)
                         :slant (font-discovery:slant font))))

(defmethod simple:request-font ((renderer renderer) (family pathname) &key)
  (simple:request-font renderer (make-instance 'font :file family)))

(defmethod simple:request-font ((renderer renderer) (default (eql :default)) &key)
  (simple:request-font renderer "Arial"))

(defun compute-text (font text s)
  (let ((array (make-array (* 6 4 (length text))))
        (i 0) (minx 0) (miny 0) (maxx 0) (maxy 0))
    (labels ((vertex (x y u v)
               (setf (aref array (+ i 0)) x)
               (setf (aref array (+ i 1)) y)
               (setf (aref array (+ i 2)) u)
               (setf (aref array (+ i 3)) v)
               (incf i 4))
             (thunk (x- y- x+ y+ u- v- u+ v+)
               (setf x- (* s x-))
               (setf y- (* s y-))
               (setf x+ (* s x+))
               (setf y+ (* s y+))
               (setf minx (min minx x-))
               (setf miny (min miny y-))
               (setf maxx (max maxx x+))
               (setf maxy (max maxy y+))
               (vertex x- y+ u- (- 1 v-))
               (vertex x- y- u- (- 1 v+))
               (vertex x+ y+ u+ (- 1 v-))
               (vertex x+ y+ u+ (- 1 v-))
               (vertex x- y- u- (- 1 v+))
               (vertex x+ y- u+ (- 1 v+))))
      (3b-bmfont:map-glyphs (data font) #'thunk text))
    (values array minx miny maxx maxy)))

(defclass text (simple:text)
  ((vertex-data)
   (dimensions)))

(defmethod scale ((text text))
  (/ (alloy:to-px (simple:size text)) (3b-bmfont:size (font text))))

(defmethod shared-initialize :after ((text text) slots &key)
  (alloy:allocate (font text))
  (let ((s (scale text)))
    (multiple-value-bind (array x- y- x+ y+) (compute-text (font text) (text text) s)
      (let ((p (simple:resolve-alignment (simple:bounds text) (simple:halign text) (simple:valign text)
                                         (alloy:px-size (- x+ x-) (- y+ y-)))))
        (setf (vertex-data text) array)
        (setf (dimensions text) (alloy:px-extent (alloy:pxx p) (alloy:pxy p) (- x+ x-) (- y+ y-)))))))

(defmethod alloy:render ((renderer renderer) (shape text))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (atlas (font shape)))
  (let ((shader (opengl:resource 'text-shader renderer))
        (vbo (opengl:resource 'text-vbo renderer))
        (vao (opengl:resource 'text-vao renderer)))
    (simple:with-pushed-transforms (renderer)
      (simple:translate renderer (dimensions text))
      (setf (opengl:uniform shader "transform") (simple:transform-matrix renderer))
      (setf (opengl:uniform shader "color") (simple:pattern shape))
      (opengl:update-vertex-buffer vbo (vertex-data shape))
      (opengl:draw-vertex-array vao :triangles (/ (vertex-data shape) 3 2 4)))))

(defmethod simple:ideal-bounds ((text text))
  (dimensions text))

(defclass cursor (simple:cursor) ())

(defmethod simple:cursor ((renderer renderer) (text text) (start integer) &rest initargs)
  (apply #'make-instance 'cursor :text text :start start initargs))

(defmethod shared-initialize :after ((cursor cursor) slots &key)
  (let ((text (simple:text-object cursor))
        (x 0))
    (labels ((thunk (x- y- x+ y+ u- v- u+ v+)
               (declare (ignore x- y- y+ u- v- u+ v+))
               (setf x x+)))
      (3b-bmfont:map-glyphs (data (font text)) #'thunk text :end (position cursor)))
    (let ((s (scale text))
          (d (dimensions text)))
      (setf (simple:bounds cursor) (alloy:px-extent (* s x) 0 (* s 2) (alloy:pxh d))))))

(defclass selection (simple:selection) ())

(defmethod simple:selection ((renderer renderer) (text text) (start integer) (end integer) &rest initargs)
  (apply #'make-instance 'selection :text text :start start :end end initargs))

(defmethod shared-initialize :after ((selection selection) slots &key)
  (let ((text (simple:text-object selection))
        (minx 0))
    (labels ((thunk (x- y- x+ y+ u- v- u+ v+)
               (declare (ignore x- y- y+ u- v- u+ v+))
               (setf minx x+)))
      (3b-bmfont:map-glyphs (data (font text)) #'thunk text :end (start selection)))
    (let ((maxw 0))
      (labels ((thunk (x- y- x+ y+ u- v- u+ v+)
                 (declare (ignore x- y- y+ u- v- u+ v+))
                 (setf maxw x+)))
        (3b-bmfont:map-glyphs (data (font text)) #'thunk text :start (start selection) :end (end selection)))
      (let ((s (scale text))
            (d (dimensions text)))
        (setf (simple:bounds selection) (alloy:px-extent (* s minx) (pxy d) (* s maxw) (pxh d)))))))
