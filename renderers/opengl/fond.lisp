#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.renderers.opengl.fond
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl)
   (#:font-discovery #:org.shirakumo.font-discovery)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors))
  (:export
   #:*default-charset*
   #:renderer
   #:font
   #:charset
   #:base-size))
(in-package #:org.shirakumo.alloy.renderers.opengl.fond)

(defparameter *default-charset*
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")

(defclass renderer (opengl:renderer)
  ((font-cache :initform () :accessor font-cache)))

(defmethod alloy:allocate :before ((renderer renderer))
  (setf (opengl:resource 'text-vbo renderer)
        (opengl:make-vertex-buffer renderer (make-array 0 :element-type 'single-float)
                                   :data-usage :dynamic-draw))
  (setf (opengl:resource 'text-ebo renderer)
        (opengl:make-vertex-buffer renderer (make-array 0 :element-type 'single-float)
                                   :buffer-type :element-array-buffer
                                   :data-usage :dynamic-draw))
  (setf (opengl:resource 'text-vao renderer)
        (opengl:make-vertex-array renderer `((,(opengl:resource 'text-vbo renderer) :size 2 :stride 16 :offset 0)
                                             (,(opengl:resource 'text-vbo renderer) :size 2 :stride 16 :offset 8)
                                             ,(opengl:resource 'text-ebo renderer))))
  (setf (opengl:resource 'text-shader renderer)
        (opengl:make-shader renderer
                            :vertex-shader "#version 330 core
layout (location=0) in vec2 pos;
layout (location=1) in vec2 in_uv;
uniform mat3 transform;
out vec2 uv;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1), 1);
  uv = in_uv;
}"
                            :fragment-shader "#version 330 core
uniform sampler2D image;
uniform vec4 color;
in vec2 uv;
out vec4 out_color;

void main(){
  out_color = color * texture(image, uv, -0.65).r;
}")))

(defmethod alloy:deallocate :after ((renderer renderer))
  (setf (font-cache renderer) ()))

(defclass font (simple:font)
  ((atlas :accessor atlas)
   (charset :initarg :charset :initform *default-charset* :reader charset)
   (base-size :initarg :base-size :initform 24 :reader base-size)))

(defmethod ensure-font ((font simple:font) (renderer renderer))
  (loop for f in (font-cache renderer)
        do (when (or (eq font f)
                     (font= font f))
             (return f))
        finally (push (etypecase font
                        (font font)
                        (simple:font (change-class font 'font)))
                      (font-cache renderer))
                (return font)))

(defmethod font= ((a simple:font) (b simple:font))
  (and (equal (simple:family a) (simple:family b))
       (eql (simple:slant a) (simple:slant b))
       (eql (simple:spacing a) (simple:spacing b))
       (eql (simple:weight a) (simple:weight b))
       (eql (simple:stretch a) (simple:stretch b))))

(defmethod font= ((a font) (b font))
  (and (call-next-method)
       (string= (charset a) (charset b))
       (= (base-size a) (base-size b))))

(defmethod alloy:allocate ((font font))
  (unless (slot-boundp font 'atlas)
    (setf (atlas font) (cl-fond:make-font (simple:family font) (charset font)
                                          :size (base-size font)
                                          :oversample 2))))

(defmethod alloy:deallocate ((font font))
  (when (slot-boundp font 'atlas)
    (cl-fond:free (atlas font))
    (slot-makunbound font 'atlas)))

(defmethod simple:request-font ((renderer renderer) (family string) &key slant spacing weight stretch)
  (ensure-font
   (cond ((equal "" family)
          (simple:request-font renderer :default))
         (T
          (let ((font (font-discovery:find-font
                       :family family
                       :weight weight
                       :stretch stretch
                       :spacing spacing
                       :slant slant)))
            (make-instance 'font :family (font-discovery:file font)
                                 :weight (font-discovery:weight font)
                                 :stretch (font-discovery:stretch font)
                                 :spacing (font-discovery:spacing font)
                                 :slant (font-discovery:slant font)))))
   renderer))

(defmethod simple:request-font ((renderer renderer) (family pathname) &key)
  (simple:request-font renderer (make-instance 'font :family family)))

(defmethod simple:request-font ((renderer renderer) (default (eql :default)) &key)
  (simple:request-font renderer "Arial"))

(defun text-point (text scale)
  (destructuring-bind (&key l r ((:t u)) b gap) (cl-fond:compute-extent (atlas (simple:font text)) (alloy:text text))
    (declare (ignore gap))
    (ecase (simple:direction text)
      (:right
       (simple:resolve-alignment (simple:bounds text) (simple:halign text) (simple:valign text)
                                 (alloy:px-size (* scale (- r l)) (* scale (- u b))))))))

(defmethod alloy:render ((renderer renderer) (shape simple:text))
  (alloy:allocate (simple:font shape))
  (let ((atlas (atlas (simple:font shape)))
        (shader (opengl:resource 'text-shader renderer)))
    (opengl:bind shader)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (cl-fond:texture atlas))
    (simple:with-pushed-transforms (renderer)
      (let ((s (* 2 (/ (alloy:to-px (simple:size shape)) (cl-fond:size atlas)))))
        (simple:translate renderer (text-point shape s))
        (simple:scale renderer (alloy:px-size s s)))
      (setf (opengl:uniform shader "transform") (simple:transform-matrix renderer)))
    (setf (opengl:uniform shader "color") (simple:pattern shape))
    ;; Could cache this by allocating a vbo/ebo per text. Don't have a deallocation API though, so...
    (let ((count (cl-fond:update-text atlas (alloy:text shape)
                                      (opengl:gl-name (opengl:resource 'text-vbo renderer))
                                      (opengl:gl-name (opengl:resource 'text-ebo renderer)))))
      (opengl:draw-vertex-array (opengl:resource 'text-vao renderer) :triangles count))))

;; FIXME: bad
(defmethod simple:ideal-bounds ((text simple:text))
  (alloy:allocate (simple:font text))
  (destructuring-bind (&key l r ((:t u)) b gap) (cl-fond:compute-extent (atlas (simple:font text)) (alloy:text text))
    (declare (ignore gap))
    (let ((s (* 3 (/ (alloy:to-px (simple:size text)) (cl-fond:size (atlas (simple:font text)))))))
      (alloy:px-extent (* l s) (* u s) (* s (+ l r)) (* s (+ u b))))))

(defclass cursor (simple:filled-rectangle)
  ((text :initarg :text :accessor text)
   (simple:bounds :initform NIL)))

(defmethod shared-initialize :after ((cursor cursor) slots &key position)
  (when position
    (alloy:allocate (simple:font (text cursor)))
    (destructuring-bind (&key l r ((:t u)) b gap) (cl-fond:compute-extent (atlas (simple:font (text cursor))) (alloy:text (text cursor))
                                                                          :end position)
      (declare (ignore gap))
      (let ((s (* 2 (/ (alloy:to-px (simple:size (text cursor))) (cl-fond:size (atlas (simple:font (text cursor))))))))
        ;; FIXME: handle alignment of the cursor depending on text alignment
        (setf (simple:bounds cursor) (alloy:px-extent (* s (- r l)) (* s b) 2 (* s u)))))))

(defmethod simple:cursor ((renderer renderer) (text simple:text) position &rest initargs)
  (apply #'make-instance 'cursor :text text :position position initargs))

(defclass selection (simple:filled-rectangle)
  ((text :initarg :text :accessor text)
   (start :initarg :start :accessor start)
   (end :initarg :end :accessor end)
   (simple:pattern :initform (colored:color 0 0 1 0.5))
   (simple:bounds :initform NIL)))

(defmethod shared-initialize :after ((selection selection) slots &key start end)
  (when (or start end)
    (let ((text (text selection)))
      ;; FIXME: This will not work with multiline.
      (destructuring-bind (&key l r ((:t u)) b gap) (cl-fond:compute-extent (atlas (simple:font text)) (alloy:text text)
                                                                            :end (or start (start selection)))
        (declare (ignore gap))
        (destructuring-bind (&key ((:r r2)) &allow-other-keys) (cl-fond:compute-extent (atlas (simple:font text)) (alloy:text text)
                                                                                :end (or end (end selection)))
          (let ((s (* 2 (/ (alloy:to-px (simple:size (text selection))) (cl-fond:size (atlas (simple:font (text selection))))))))
            (setf (simple:bounds selection) (alloy:px-extent (* s (- r l)) (* s b) (* s (- r2 r)) (* s (+ u b))))))))))

(defmethod simple:selection ((renderer renderer) (text simple:text) start end &rest initargs)
  (apply #'make-instance 'selection :text text :start start :end end initargs))
