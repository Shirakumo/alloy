#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.opengl)

(defparameter *circ-polycount* 36)

;; TODO: With a UBO we could avoid having to re-set uniforms at ever draw
;;       and instead change them when the style is set.

(defclass renderer (simple:transformed-renderer)
  ((resources :initform (make-hash-table :test 'equal) :reader resources)))

(defmethod resource (name (renderer renderer) &optional (errorp T))
  (or (gethash name (resources renderer))
      (when errorp
        (error "The name~%  ~s~%is not allocated to any resource."
               name))))

(defmethod (setf resource) (value name (renderer renderer))
  (setf (gethash name (resources renderer)) value))

(defun make-line-array (points)
  (let ((array (make-array (max 0 (* 6 4 (1- (length points)))) :element-type 'single-float))
        (i -1))
    (labels ((vertex (p nx ny)
               (setf (aref array (incf i)) (alloy:pxx p))
               (setf (aref array (incf i)) (alloy:pxy p))
               (setf (aref array (incf i)) nx)
               (setf (aref array (incf i)) ny))
             (line (a b)
               (let* ((ux (- (- (alloy:pxy b) (alloy:pxy a))))
                      (uy (- (alloy:pxx b) (alloy:pxx a)))
                      (len (sqrt (+ (* ux ux) (* uy uy)))))
                 (setf ux (/ ux len))
                 (setf uy (/ uy len))
                 (vertex a (- ux) (- uy))
                 (vertex b (- ux) (- uy))
                 (vertex a (+ ux) (+ uy))
                 (vertex b (- ux) (- uy))
                 (vertex b (+ ux) (+ uy))
                 (vertex a (+ ux) (+ uy)))))
      (etypecase points
        (list
         (loop for (a b) on points
               while b do (line a b)))
        (vector
         (loop for i from 0 below (1- (length points))
               do (line (aref points i) (aref points (1+ i))))))
      array)))

(defmethod alloy:allocate :before ((renderer renderer))
  ;; FIXME: Implement sharing between renderers.
  ;; Allocate the necessary geometry.
  (flet ((arr (&rest data)
           (make-array (length data) :element-type 'single-float :initial-contents data))
         (make-geometry (vbo vao content &key (data-usage :static-draw) (bindings `((:size 2 :offset 0 :stride 8))))
           (setf (resource vbo renderer) (make-vertex-buffer renderer content :data-usage data-usage))
           (setf (resource vao renderer) (make-vertex-array renderer (loop for binding in bindings
                                                                           collect (list* (resource vbo renderer) binding))))))
    ;; lines
    (make-geometry 'rect-line-vbo 'rect-line-vao
                   (make-line-array (list (alloy:px-point 0f0 0f0)
                                          (alloy:px-point 0f0 1f0)
                                          (alloy:px-point 1f0 1f0)
                                          (alloy:px-point 1f0 0f0)
                                          (alloy:px-point 0f0 0f0)))
                   :bindings '((:size 2 :offset 0 :stride 16) (:size 2 :offset 8 :stride 16)))
    (make-geometry 'circ-line-vbo 'circ-line-vao
                   (make-line-array (loop for i from 0 to *circ-polycount*
                                          for tt = (* i (/ *circ-polycount*) 2 PI)
                                          collect (alloy:px-point (float (cos tt) 0f0) (float (sin tt) 0f0))))
                   :bindings '((:size 2 :offset 0 :stride 16) (:size 2 :offset 8 :stride 16)))
    (make-geometry 'line-vbo 'line-vao (arr)
                   :data-usage :stream-draw
                   :bindings '((:size 2 :offset 0 :stride 16) (:size 2 :offset 8 :stride 16)))
    ;; :triangles
    (make-geometry 'rect-fill-vbo 'rect-fill-vao
                   (arr 0f0 0f0  1f0 1f0  0f0 1f0
                        0f0 0f0  1f0 0f0  1f0 1f0))
    (make-geometry 'circ-fill-vbo 'circ-fill-vao
                   (coerce
                    (list* 0f0 0f0
                           (loop for i from 0 to *circ-polycount*
                                 for tt = (* i (/ *circ-polycount*) 2 PI)
                                 collect (float (cos tt) 0f0)
                                 collect (float (sin tt) 0f0)))
                    '(vector single-float)))
    (make-geometry 'stream-vbo 'stream-vao (arr)
                   :data-usage :stream-draw)
    (make-geometry 'gradient-vbo 'gradient-vao (arr)
                   :data-usage :stream-draw
                   :bindings '((:size 2 :offset 0 :stride 24) (:size 4 :offset 8 :stride 24))))

  ;; Allocate the necessary shaders.
  (flet ((make-shader (name vert frag)
           (setf (resource name renderer) (make-shader renderer :vertex-shader vert :fragment-shader frag))))
    (make-shader 'line-shader
                 "#version 330 core
layout(location = 0) in vec2 position;
layout(location = 1) in vec2 normal;

out vec2 line_normal;
uniform float line_width = 3.0;
uniform mat3 transform;
uniform vec2 view_size;

void main(){
  vec3 delta = vec3(normal * line_width, 0);
  delta.xy /= view_size;
  vec3 pos = transform*vec3(position, 1);
  gl_Position = vec4(pos + delta, 1);
  line_normal = normal;
}"
                 "#version 330 core
in vec2 line_normal;
uniform float feather = 0.66;
uniform vec4 color;
out vec4 out_color;

void main(){
   float strength = 1-length(line_normal);
   out_color = color * clamp(strength*feather+feather, 0, 1);
}")
    (make-shader 'gradient-shader
                 "#version 330 core
layout (location=0) in vec2 pos;
layout (location=1) in vec4 vertex_color;

out vec4 color;
uniform mat3 transform;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1), 1);
  color = vertex_color;
}"
                 "#version 330 core
in vec4 color;
out vec4 out_color;

void main(){
  out_color = color;
}")
    (make-shader 'basic-shader
                 "#version 330 core
layout (location=0) in vec2 pos;
uniform mat3 transform;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1), 1);
}"
                 "#version 330 core
uniform vec4 color;
out vec4 out_color;

void main(){
  out_color = color;
}")
    (make-shader 'image-shader
                 "#version 330 core
layout (location=0) in vec2 pos;
uniform mat3 transform;
out vec2 uv;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1), 1);
  uv = pos;
}"
                 "#version 330 core
uniform sampler2D image;
in vec2 uv;
out vec4 out_color;

void main(){
  out_color = texture(image, uv);
}")))

(defmethod alloy:allocate ((renderer renderer))
  (loop for resource being the hash-values of (resources renderer)
        do (with-simple-restart (continue "Ignore the failed allocation.")
             (alloy:allocate resource))))

(defmethod alloy:deallocate ((renderer renderer))
  (loop for resource being the hash-values of (resources renderer)
        do (with-simple-restart (continue "Ignore the failed deallocation.")
             (alloy:deallocate resource)))
  (clrhash (resources renderer)))

(defmethod alloy:register (renderable (renderer renderer)))

(defvar *clip-depth* 0)

(defmethod simple:clip ((renderer renderer) (shape simple:shape))
  ;; FIXME: THIS NEEDS TO BE FIXED
  ;; (incf *clip-depth* 1)
  ;; (gl:stencil-op :keep :incr :incr)
  ;; (gl:stencil-func :always 1 #xFF)
  ;; (gl:color-mask NIL NIL NIL NIL)
  ;; (gl:depth-mask NIL)
  ;; (unwind-protect
  ;;      (alloy:render renderer shape)
  ;;   (gl:stencil-op :keep :keep :keep)
  ;;   (gl:stencil-func :gequal *clip-depth* #xFF)
  ;;   (gl:color-mask T T T T)
  ;;   (gl:depth-mask T))
  )

(defmethod simple:clip ((renderer renderer) (extent alloy:extent))
  ;; FIXME: avoid allocation of rectangle
  (simple:clip renderer (simple:rectangle renderer extent)))

(defmethod simple:call-with-pushed-transforms :around (function (renderer renderer))
  (let ((*clip-depth* *clip-depth*))
    (call-next-method))
  (gl:stencil-func :gequal *clip-depth* #xFF))

(defmethod simple:clear ((renderer renderer) extent)
  (let ((mode (simple:composite-mode renderer)))
    (setf (simple:composite-mode renderer) :clear)
    (unwind-protect
         (simple:rectangle renderer extent)
      (setf (simple:composite-mode renderer) mode))))

(defmethod (setf simple:composite-mode) :before (mode (renderer renderer))
  (ecase mode
    (:source-over
     (gl:blend-func :src-alpha :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination-over
     (gl:blend-func :one-minus-dst-alpha :one)
     (gl:blend-equation :func-add))
    (:clear
     (gl:blend-func :zero :zero)
     (gl:blend-equation :func-add))
    (:source
     (gl:blend-func :one :zero)
     (gl:blend-equation :func-add))
    (:destination
     (gl:blend-func :zero :one)
     (gl:blend-equation :func-add))
    (:source-in
     (gl:blend-func :dst-alpha :zero)
     (gl:blend-equation :func-add))
    (:destination-in
     (gl:blend-func :zero :src-alpha)
     (gl:blend-equation :func-add))
    (:source-out
     (gl:blend-func :one-minus-dst-alpha :zero)
     (gl:blend-equation :func-add))
    (:destination-out
     (gl:blend-func :zero :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination-atop
     (gl:blend-func :one-minus-dst-alpha :src-alpha)
     (gl:blend-equation :func-add))
    (:xor
     (gl:blend-func :one-minus-dst-alpha :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:add
     (gl:blend-func :one :one)
     (gl:blend-equation :func-add))
    (:multiply
     (gl:blend-func :dst-color :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:screen
     (gl:blend-func :one :one-minus-src-color)
     (gl:blend-equation :func-add))
    (:darken
     (gl:blend-func :one :one)
     (gl:blend-equation :func-max))
    (:difference
     (gl:blend-func :one :one)
     (gl:blend-equation :func-subtract))
    (:invert
     (gl:blend-func :one :one)
     (gl:blend-equation :func-reverse-subtract))))

(defclass line-strip (simple:line-strip)
  ((data :accessor data)))

(defmethod shared-initialize :after ((shape line-strip) slots &key (points NIL points-p))
  (when points-p
    (setf (data shape) (make-line-array points))))

(defmethod simple:line-strip ((renderer renderer) (points vector) &rest initargs)
  (apply #'make-instance 'line-strip :points points initargs))

(defmethod alloy:render ((renderer renderer) (shape line-strip))
  (let ((shader (resource 'line-shader renderer))
        (data (data shape)))
    (update-vertex-buffer (resource 'line-vbo renderer) data)
    (bind shader)
    (setf (uniform shader "transform") (simple:transform-matrix renderer))
    (setf (uniform shader "color") (simple:pattern shape))
    (setf (uniform shader "line_width") (alloy:to-px (simple:line-width shape)))
    (setf (uniform shader "view_size") (view-size renderer))
    (draw-vertex-array (resource 'line-vao renderer) :triangles (/ (length data) 4))))

(defclass curve (line-strip)
  ())

(defmethod shared-initialize :around ((shape curve) slots &rest initargs &key points (segments 20))
  ;; TODO: Do this GPU-side with tesselation
  (let ((lines (make-array (* (1+ segments) (/ (1- (length points)) 3))))
        (i 0))
    (declare (type (signed-byte 32) i segments))
    (labels ((bezier (tt x1 x2 x3 x4)
               (declare (optimize speed))
               (declare (type single-float tt x1 x2 x3 x4))
               (+ (* x1 (expt (- 1 tt) 3))
                  (* x2 3 tt (expt (- 1 tt) 2))
                  (* x3 3 (expt tt 2) (- 1 tt))
                  (* x4 (expt tt 3))))
             (curve (a b c d)
               (declare (optimize speed))
               (loop for tt from 0f0 by (/ 1f0 segments)
                     for x = (bezier tt (alloy:pxx a) (alloy:pxx b) (alloy:pxx c) (alloy:pxx d))
                     for y = (bezier tt (alloy:pxy a) (alloy:pxy b) (alloy:pxy c) (alloy:pxy d))
                     repeat (1+ segments)
                     do (setf (aref lines i) (alloy:px-point x y))
                        (incf i))))
      (etypecase points
        (list (loop for (a b c d) on points by #'cdddr
                    do (curve a b c d)))
        (vector (loop for j from 0 below (1- (length points)) by 3
                      do (curve (aref points (+ j 0)) (aref points (+ j 1)) (aref points (+ j 2)) (aref points (+ j 3))))))
      (apply #'call-next-method shape slots :points lines initargs))))

(defmethod simple:curve ((renderer renderer) (points vector) &rest initargs)
  (apply #'make-instance 'curve :points points initargs))

(defmethod simple:curve ((renderer renderer) (points cons) &rest initargs)
  (apply #'make-instance 'curve :points points initargs))

(defmethod alloy:render ((renderer renderer) (shape simple:filled-rectangle))
  (let ((shader (resource 'basic-shader renderer)))
    (bind shader)
    (simple:with-pushed-transforms (renderer)
      (simple:translate renderer (simple:bounds shape))
      (simple:scale renderer (simple:bounds shape))
      (setf (uniform shader "transform") (simple:transform-matrix renderer)))
    (setf (uniform shader "color") (simple:pattern shape))
    (setf (uniform shader "view_size") (view-size renderer))
    (draw-vertex-array (resource 'rect-fill-vao renderer) :triangles 6)))

(defmethod alloy:render ((renderer renderer) (shape simple:outlined-rectangle))
  (let ((shader (resource 'line-shader renderer)))
    (bind shader)
    (simple:with-pushed-transforms (renderer)
      (simple:translate renderer (simple:bounds shape))
      (simple:scale renderer (simple:bounds shape))
      (setf (uniform shader "transform") (simple:transform-matrix renderer)))
    (setf (uniform shader "color") (simple:pattern shape))
    (setf (uniform shader "line_width") (alloy:to-px (simple:line-width shape)))
    (setf (uniform shader "view_size") (view-size renderer))
    (draw-vertex-array (resource 'rect-line-vao renderer) :triangles 24)))

(defmethod alloy:render ((renderer renderer) (shape simple:filled-ellipse))
  (let ((shader (resource 'basic-shader renderer))
        (extent (alloy:ensure-extent (simple:bounds shape))))
    (bind shader)
    (simple:with-pushed-transforms (renderer)
      (let ((w (/ (alloy:pxw extent) 2))
            (h (/ (alloy:pxh extent) 2)))
        (simple:translate renderer (alloy:point (+ (alloy:pxx extent) w)
                                                (+ (alloy:pxy extent) h)))
        (simple:scale renderer (alloy:size w h)))
      (setf (uniform shader "transform") (simple:transform-matrix renderer)))
    (setf (uniform shader "color") (simple:pattern shape))
    (setf (uniform shader "view_size") (view-size renderer))
    (draw-vertex-array (resource 'circ-fill-vao renderer) :triangle-fan (+ *circ-polycount* 2))))

(defmethod alloy:render ((renderer renderer) (shape simple:outlined-ellipse))
  (let ((shader (resource 'line-shader renderer))
        (extent (alloy:ensure-extent (simple:bounds shape))))
    (bind shader)
    (simple:with-pushed-transforms (renderer)
      (let ((w (/ (alloy:pxw extent) 2))
            (h (/ (alloy:pxh extent) 2)))
        (simple:translate renderer (alloy:point (+ (alloy:pxx extent) w)
                                                (+ (alloy:pxy extent) h)))
        (simple:scale renderer (alloy:size w h)))
      (setf (uniform shader "transform") (simple:transform-matrix renderer)))
    (setf (uniform shader "color") (simple:pattern shape))
    (setf (uniform shader "line_width") (alloy:to-px (simple:line-width shape)))
    (setf (uniform shader "view_size") (view-size renderer))
    (draw-vertex-array (resource 'circ-line-vao renderer) :triangles (* *circ-polycount* 6))))

(defclass polygon (simple:polygon)
  ((data :reader data)))

;;; FIXME: this will not run if the user just changes size and does not reinitialise the shape.
;;;        in turn the units will not match up any longer. WE might need to specify this in the
;;;        simple protocol if there's no automated way to determine when to recompute
(defmethod shared-initialize :after ((shape polygon) slots &key (points NIL points-p))
  (when points-p
    (let ((data (make-array (* 3 (length points)))))
      (loop with i = -1
            for point in points
            do (setf (aref data (incf i)) (alloy:pxx point))
               (setf (aref data (incf i)) (alloy:pxy point)))
      (setf (data shape) data))))

(defmethod simple:polygon ((renderer renderer) (points vector) &rest initargs)
  (apply #'make-instance 'polygon :points points initargs))

(defmethod alloy:render ((renderer renderer) (shape polygon))
  (let ((shader (resource 'basic-shader renderer))
        (data (data shape)))
    (update-vertex-buffer (resource 'stream-vbo renderer) data)
    (bind shader)
    (setf (uniform shader "transform") (simple:transform-matrix renderer))
    (setf (uniform shader "color") (simple:pattern shape))
    (draw-vertex-array (resource 'stream-vao renderer) :triangle-fan (/ (length data) 2))))

(defmethod alloy:render ((renderer renderer) (icon simple:icon))
  (let ((shader (resource 'image-shader renderer)))
    ;; FIXME: alignment
    (bind shader)
    (bind (simple:image icon))
    (simple:with-pushed-transforms (renderer)
      (simple:translate renderer (simple:bounds icon))
      (simple:scale renderer (simple:size icon))
      (setf (uniform shader "transform") (simple:transform-matrix renderer)))
    (draw-vertex-array (resource 'rect-fill-vao renderer) :triangles 6)))

(defmethod simple:request-image ((renderer renderer) data &key size)
  (make-texture renderer (alloy:pxw size) (alloy:pxh size) data))

