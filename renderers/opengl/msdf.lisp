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
  (:export
   #:fontcache-default-directory
   #:renderer
   #:fontcache-directory
   #:font
   #:file
   #:cached-font
   #:text
   #:cursor
   #:selection))
(in-package #:org.shirakumo.alloy.renderers.opengl.msdf)

(defun fontcache-default-directory ()
  ;; KLUDGE: Would be better to use the LOCALAPPDATA envvar, but that requires portable getenv.
  #+win32 (merge-pathnames "AppData/Local/alloy/msdf/" (user-homedir-pathname))
  #-win32 (merge-pathnames ".cache/alloy/msdf/" (user-homedir-pathname)))

(defun fontcache-file (path cache)
  (cond ((or (string= (pathname-type path) "font")
             (string= (pathname-type path) "fnt"))
         path)
        ((or (string= (pathname-type path) "otf")
             (string= (pathname-type path) "ttf"))
         ;; KLUDGE: would be better to name the files by font properties to collate duplicate files
         ;;         and to avoid pathname length or depth overflow.
         (merge-pathnames (make-pathname :type "fnt"
                                         :directory (list :relative)
                                         :defaults path)
                          cache))
        (T
         (error "Unrecognised font format for file~%  ~a" path))))

(defclass renderer (opengl:renderer)
  ((fontcache-directory :initarg :fontcache :initform (fontcache-default-directory) :accessor fontcache-directory)
   (fontcache :initform (make-hash-table :test 'equal) :accessor fontcache)
   (font-name-cache :initform (make-hash-table :test 'equalp) :accessor font-name-cache)))

(defmethod initialize-instance :after ((renderer renderer) &key)
  ;; Init font cache
  (dolist (path (directory (make-pathname :name :wild :type "fnt" :defaults (fontcache-directory renderer))))
    (setf (gethash (pathname-name path) (font-name-cache renderer)) path)))

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

float median(float r, float g, float b){
  return max(min(r, g), min(max(r, g), b));
}

void main(){
  vec2 msdfUnit = pxRange/vec2(textureSize(image, 0));
  vec3 msdfData = texture(image, uv).rgb;
  float sigDist = median(msdfData.r, msdfData.g, msdfData.b) - 0.5;
  sigDist *= dot(msdfUnit, 0.5/fwidth(uv));
  float opacity = clamp(sigDist + 0.5, 0.0, 1.0);
  out_color = vec4(color.rgb, color.a*opacity);
}")))

(defmethod alloy:deallocate ((renderer renderer))
  (loop for font being the hash-values of (fontcache renderer)
        do (alloy:deallocate font)))

(defmethod cached-font ((renderer renderer) file &optional family)
  (unless (probe-file file)
    (error "Specified font file does not exist or is not accessible.~%  ~a" file))
  (let ((cache (if family
                   (make-pathname :name family :type "fnt" :defaults (fontcache-directory renderer))
                   (fontcache-file file (fontcache-directory renderer)))))
    (unless (probe-file cache)
      (ensure-directories-exist cache)
      ;; FIXME: Use lisp-native solution and generate all available glyphs in the font
      (sb-ext:run-program "msdf-bmfont" (list "-o" (namestring cache) "-f" "json" "-s" "64" (namestring file)) :search T)
      (rename-file (make-pathname :type "json" :defaults cache) cache))
    (or (gethash cache (fontcache renderer))
        (setf (gethash cache (fontcache renderer))
              (make-instance 'font :family NIL :file cache :renderer renderer)))))

(defclass font (simple:font)
  ((renderer :initarg :renderer :accessor renderer)
   (file :initarg :file :accessor file)
   (data :accessor data)
   (atlas :accessor atlas)))

(defmethod alloy:allocate ((font font))
  (let ((renderer (renderer font)))
    (unless (slot-boundp font 'data)
      (setf (data font) (3b-bmfont:read-bmfont (file font))))
    (unless (slot-boundp font 'atlas)
      (let ((file (merge-pathnames (getf (aref (3b-bmfont:pages (data font)) 0) :file) (file font))))
        (setf (atlas font) (simple:request-image renderer file :filtering :linear))))))

(defmethod alloy:deallocate ((font font))
  (slot-makunbound font 'atlas))

(defmethod simple:request-font ((renderer renderer) (family string) &rest args)
  (let ((font (or (gethash family (font-name-cache renderer))
                  (setf (gethash family (font-name-cache renderer))
                        (font-discovery:file (apply #'font-discovery:find-font :family family args))))))
    (cached-font renderer font family)))

(defmethod simple:request-font ((renderer renderer) (file pathname) &key)
  (cached-font renderer file))

(defmethod simple:request-font ((renderer renderer) (default (eql :default)) &key)
  (simple:request-font renderer "Arial"))

(defun compute-text (font text s)
  (let ((array (make-array (* 6 4 (length text)) :element-type 'single-float))
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
      (3b-bmfont:map-glyphs (data font) #'thunk text :model-y-up T))
    (values array minx miny maxx maxy)))

(defclass text (simple:text)
  ((vertex-data :accessor vertex-data)
   (dimensions :accessor dimensions)))

(defmethod scale ((text text))
  (/ (alloy:to-px (simple:size text)) (3b-bmfont:base (data (simple:font text)))))

(defmethod shared-initialize :after ((text text) slots &key)
  (alloy:allocate (simple:font text))
  (let ((s (scale text)))
    (multiple-value-bind (array x- y- x+ y+) (compute-text (simple:font text) (alloy:text text) s)
      (declare (ignore y- y+))
      (let* ((h (* s (3b-bmfont:line-height (data (simple:font text)))))
             (p (simple:resolve-alignment (simple:bounds text) (simple:halign text) (simple:valign text)
                                          (alloy:px-size (- x+ x-) h))))
        (setf (vertex-data text) array)
        (setf (dimensions text) (alloy:px-extent (alloy:pxx p) (alloy:pxy p) (- x+ x-) h))))))

(defmethod simple:text ((renderer renderer) bounds string &rest args &key font)
  (apply #'make-instance 'text :text string :bounds bounds :font (or font (simple:request-font renderer :default)) args))

(defmethod alloy:render ((renderer renderer) (shape text))
  (opengl:bind (atlas (simple:font shape)))
  (let ((shader (opengl:resource 'text-shader renderer))
        (vbo (opengl:resource 'text-vbo renderer))
        (vao (opengl:resource 'text-vao renderer)))
    (simple:with-pushed-transforms (renderer)
      (opengl:bind shader)
      (simple:translate renderer (dimensions shape))
      (setf (opengl:uniform shader "transform") (simple:transform-matrix renderer))
      (setf (opengl:uniform shader "color") (simple:pattern shape))
      (opengl:update-vertex-buffer vbo (vertex-data shape))
      (opengl:draw-vertex-array vao :triangles (/ (length (vertex-data shape)) 4)))))

(defmethod simple:ideal-bounds ((text text))
  (dimensions text))

(defclass cursor (simple:cursor) ())

(defmethod simple:cursor ((renderer renderer) (text text) (start integer) &rest initargs)
  (apply #'make-instance 'cursor :text text :start start initargs))

(defmethod shared-initialize :after ((cursor cursor) slots &key)
  (let ((text (simple:text-object cursor)))
    (let ((x (3b-bmfont:measure-glyphs (data (simple:font text)) (alloy:text text) :end (simple:start cursor)))
          (s (scale text))
          (d (dimensions text)))
      (setf (simple:bounds cursor) (alloy:px-extent (* s x) (alloy:pxy d) (* s 4) (alloy:pxh d))))))

(defclass selection (simple:selection) ())

(defmethod simple:selection ((renderer renderer) (text text) (start integer) (end integer) &rest initargs)
  (apply #'make-instance 'selection :text text :start start :end end initargs))

(defmethod shared-initialize :after ((selection selection) slots &key)
  (let* ((text (simple:text-object selection))
         (s (scale text))
         (d (dimensions text))
         (x (3b-bmfont:measure-glyphs (data (simple:font text)) (alloy:text text)
                                      :end (simple:start selection)))
         (w (3b-bmfont:measure-glyphs (data (simple:font text)) (alloy:text text)
                                      :start (simple:start selection) :end (simple:end selection))))
    (setf (simple:bounds selection) (alloy:px-extent (* s x) (alloy:pxy d) (* s w) (alloy:pxh d)))))
