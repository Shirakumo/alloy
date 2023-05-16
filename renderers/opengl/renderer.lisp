#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.opengl)

(defvar *clip-depth* 0)
(defvar *clip-region*)

;; TODO: With a UBO we could avoid having to re-set uniforms at ever draw
;;       and instead change them when the style is set.

(defgeneric render-direct (shape renderer color))

(defclass renderer (simple:transformed-renderer)
  ((resources :initform (make-hash-table :test 'equal) :reader resources)))

(defmethod resource (name (renderer renderer) &optional (errorp T))
  (or (gethash name (resources renderer))
      (when errorp
        (error "The name~%  ~s~%is not allocated to any resource."
               name))))

(defmethod alloy:render :around ((renderer renderer) (ui alloy:ui))
  (let ((*clip-depth* 0))
    (call-next-method)
    (setf (simple:composite-mode renderer) :source-over)))

(defmethod (setf resource) (value name (renderer renderer))
  (setf (gethash name (resources renderer)) value))

(defun make-line-array (points)
  (let ((array (make-array (max 0 (* 6 5 (1- (length points)))) :element-type 'single-float))
        (i -1) (tt 0.0))
    (labels ((vertex (p nx ny tt)
               (setf (aref array (incf i)) (alloy:pxx p))
               (setf (aref array (incf i)) (alloy:pxy p))
               (setf (aref array (incf i)) nx)
               (setf (aref array (incf i)) ny)
               (setf (aref array (incf i)) tt))
             (line (a b)
               (let* ((ux (- (- (alloy:pxy b) (alloy:pxy a))))
                      (uy (- (alloy:pxx b) (alloy:pxx a)))
                      (len (sqrt (+ (* ux ux) (* uy uy))))
                      (tt0 tt) (tt1 (+ tt len)))
                 (when (< 0 len)
                   (setf ux (/ ux len))
                   (setf uy (/ uy len))
                   (vertex a (- ux) (- uy) tt0)
                   (vertex b (- ux) (- uy) tt1)
                   (vertex a (+ ux) (+ uy) tt0)
                   (vertex b (- ux) (- uy) tt1)
                   (vertex b (+ ux) (+ uy) tt1)
                   (vertex a (+ ux) (+ uy) tt0)
                   (setf tt tt1)))))
      (etypecase points
        (list
         (loop for (a b) on points
               while b do (line a b)))
        (vector
         (loop for i from 0 below (1- (length points))
               do (line (aref points i) (aref points (1+ i))))))
      array)))

(defmethod alloy:allocate :before ((renderer renderer))
  ;; TODO: Implement sharing between renderers.
  ;; Allocate the necessary geometry.
  (flet ((arr (&rest data)
           (make-array (length data) :element-type 'single-float :initial-contents data))
         (make-geometry (vbo vao content &key (data-usage :static-draw) (bindings `((:size 2 :offset 0 :stride 8))))
           (unless (resource vbo renderer NIL)
             (setf (resource vbo renderer) (make-vertex-buffer renderer content :data-usage data-usage)))
           (unless (resource vao renderer NIL)
             (setf (resource vao renderer) (make-vertex-array renderer (loop for binding in bindings
                                                                             collect (list* (resource vbo renderer) binding)))))))
    ;; lines
    (make-geometry 'rect-line-vbo 'rect-line-vao
                   (make-line-array (list (alloy:px-point 0f0 0f0)
                                          (alloy:px-point 0f0 1f0)
                                          (alloy:px-point 1f0 1f0)
                                          (alloy:px-point 1f0 0f0)
                                          (alloy:px-point 0f0 0f0)))
                   :bindings '((:size 2 :offset 0 :stride 20)
                               (:size 2 :offset 8 :stride 20)
                               (:size 1 :offset 16 :stride 20)))
    (make-geometry 'line-vbo 'line-vao (arr)
                   :data-usage :stream-draw
                   :bindings '((:size 2 :offset 0 :stride 20)
                               (:size 2 :offset 8 :stride 20)
                               (:size 1 :offset 16 :stride 20)))
    ;; :triangles
    (make-geometry 'rect-fill-vbo 'rect-fill-vao
                   (arr 0f0 0f0  1f0 1f0  0f0 1f0
                        0f0 0f0  1f0 0f0  1f0 1f0))
    (make-geometry 'stream-vbo 'stream-vao (arr)
                   :data-usage :stream-draw)
    (make-geometry 'gradient-vbo 'gradient-vao (arr)
                   :data-usage :stream-draw
                   :bindings '((:size 2 :offset 0 :stride 24) (:size 4 :offset 8 :stride 24))))

  ;; Allocate the necessary shaders.
  (flet ((make-shader (name vert frag)
           (unless (resource name renderer NIL)
             (setf (resource name renderer) (make-shader renderer :vertex-shader vert :fragment-shader frag)))))
    (make-shader 'line-shader
                 "
layout(location = 0) in vec2 position;
layout(location = 1) in vec2 normal;
layout(location = 2) in float time;

out vec2 line_normal;
out float t;
uniform float line_width = 3.0;
uniform float gap = 0.0;
uniform mat3 transform;
uniform vec2 view_size;

void main(){
  vec3 delta = vec3(normal * line_width, 0.0);
  delta.xy /= view_size;
  vec3 pos = transform*vec3(position, 1.0);
  gl_Position = vec4(pos + delta, 1.0);
  line_normal = normal;
  t = time/(line_width*0.3)*gap;
}"
                 "
in vec2 line_normal;
in float t;
uniform float feather = 0.3;
uniform vec4 color;
out vec4 out_color;

void main(){
   out_color = color * ((1-length(line_normal))/feather) * clamp(1-sin(t)*4, 0.0, 1.0);
}")
    (make-shader 'circle-fill-shader
                 "
layout (location=0) in vec2 pos;
uniform mat3 transform;
uniform float start_angle;
uniform float end_angle;
out vec2 uv;
out vec2 c;

#define PI_2 1.5707963267948966

void main(){
  uv = pos-0.5;
  float start = start_angle-PI_2;
  float aperture = abs(end_angle-start_angle)*0.5;
  if(end_angle < start_angle){
    start += -2*(aperture+PI_2);
    aperture -= 2*PI_2;
  }

  c = vec2(sin(aperture), cos(aperture));
  float cstart = cos(aperture+start);
  float sstart = sin(aperture+start);
  uv = mat2(cstart,-sstart,
            sstart, cstart)*uv;
  gl_Position = vec4(transform*vec3(pos, 1.0), 1.0);
}"
                 "
uniform vec4 color;
in vec2 uv;
in vec2 c;
out vec4 out_color;

void main(){
  vec2 p = vec2(abs(uv.x), uv.y);
  float l = length(p)-0.5;
  float m = length(p-c*clamp(dot(p,c),0.0,0.5));
  float sdf = max(l,m*sign(c.y*p.x-c.x*p.y));
  float dsdf = fwidth(sdf)*0.5;
  sdf = smoothstep(dsdf, -dsdf, sdf);
  out_color = color*sdf;
}")
    (make-shader 'circle-line-shader
                 "
layout (location=0) in vec2 pos;
uniform mat3 transform;
uniform float start_angle;
uniform float end_angle;
out vec2 uv;
out vec2 c;

#define PI_2 1.5707963267948966

void main(){
  uv = pos-0.5;
  float start = start_angle-PI_2;
  float aperture = abs(end_angle-start_angle)*0.5;
  if(end_angle < start_angle){
    start += -2*(aperture+PI_2);
    aperture -= 2*PI_2;
  }

  c = vec2(sin(aperture), cos(aperture));
  float cstart = cos(aperture+start);
  float sstart = sin(aperture+start);
  uv = mat2(cstart,-sstart,
            sstart, cstart)*uv;
  gl_Position = vec4(transform*vec3(pos, 1.0), 1.0);
}"
                 "
uniform vec4 color;
uniform float line_width = 3.0;
in vec2 uv;
in vec2 c;
out vec4 out_color;

void main(){
  vec2 p = vec2(abs(uv.x), uv.y);
  float l = length(p)-0.5;
  float m = length(p-c*clamp(dot(p,c),0.0,0.5));
  float sdf = max(max(l,m*sign(c.y*p.x-c.x*p.y)), (0.5-line_width)-length(uv));
  float dsdf = fwidth(sdf)*0.5;
  sdf = smoothstep(dsdf, -dsdf, sdf);
  out_color = color*sdf;
}")
    (make-shader 'gradient-shader
                 "
layout (location=0) in vec2 pos;
layout (location=1) in vec4 vertex_color;

out vec4 color;
uniform mat3 transform;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1.0), 1.0);
  color = vertex_color;
}"
                 "
in vec4 color;
out vec4 out_color;

void main(){
  out_color = vec4(color.rgb*color.a, color.a);
}")
    (make-shader 'corner-shader
                 "
layout (location=0) in vec2 pos;
uniform mat3 transform;
uniform float start_angle;
uniform float end_angle;
out vec2 uv;

void main(){
  uv = pos-1.0;
  gl_Position = vec4(transform*vec3(pos, 1.0), 1.0);
}"
                 "
uniform vec4 color;
uniform float line_width = 3.0;
in vec2 uv;
out vec4 out_color;

void main(){
  float sdf = length(uv)-1.0;
  float dsdf = fwidth(sdf)*0.5;
  sdf = smoothstep(dsdf, -dsdf, sdf);
  out_color = color*sdf;
}")
    (make-shader 'basic-shader
                 "
layout (location=0) in vec2 pos;
uniform mat3 transform;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1.0), 1.0);
}"
                 "
uniform vec4 color;
out vec4 out_color;

void main(){
  out_color = vec4(color.rgb*color.a, color.a);
}")
    (make-shader 'image-shader
                 "
layout (location=0) in vec2 pos;
uniform mat3 transform;
out vec2 uv;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1.0), 1.0);
  uv = pos;
}"
                 "
uniform sampler2D image;
uniform vec2 uv_offset = vec2(0,0);
uniform vec2 uv_scale = vec2(1,1);
in vec2 uv;
out vec4 out_color;

void main(){
  vec4 color = texture(image, (uv/uv_scale)+uv_offset);
  out_color = vec4(color.rgb*color.a, color.a);
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

(defmethod alloy:render ((renderer renderer) (shape simple:patterned-shape))
  (render-direct shape renderer (simple:pattern shape)))

(defmethod alloy:render ((renderer renderer) (shape simple:shape))
  (render-direct shape renderer colors:black))

(defmethod alloy:register (renderable (renderer renderer)))

(defmethod simple:z-index ((renderer renderer))
  (call-next-method))

(defmethod (setf simple:z-index) (value (renderer renderer))
  (call-next-method value renderer))

(defmethod simple:clip ((renderer renderer) (shape simple:shape))
  (when (cdr *clip-region*)
    (error "Clipping already applied."))
  (gl:stencil-op :keep :incr :incr)
  (gl:color-mask NIL NIL NIL NIL)
  (gl:depth-mask NIL)
  ;; KLUDGE: we set this to be the bounds as the shape might be changed later.
  ;;         this however constricts us to rectangular clips. BAD!
  (setf (cdr *clip-region*) (simple:bounds shape))
  (replace (car *clip-region*) (simple:transform-matrix renderer))
  (render-direct shape renderer colors:black)
  (incf *clip-depth* 1)
  (gl:stencil-op :keep :keep :keep)
  (gl:stencil-func :lequal *clip-depth* #xFF)
  (gl:color-mask T T T T)
  (gl:depth-mask T))

(let ((rect (make-instance 'simple:filled-rectangle :bounds (alloy:extent))))
  (defmethod simple:clip ((renderer renderer) (extent alloy:extent))
    (setf (simple:bounds rect) extent)
    (simple:clip renderer rect))

  (defmethod simple:clip ((renderer renderer) (extent alloy:size))
    ;; TODO: make this not cons
    (setf (simple:bounds rect) (alloy:extent 0 0 (alloy:size-w extent) (alloy:size-h extent)))
    (simple:clip renderer rect))

  (defmethod simple:call-with-pushed-transforms :around (function (renderer renderer) &key clear)
    (cond (clear
           (call-next-method))
          (T
           (let* ((*clip-depth* *clip-depth*)
                  (arr (make-array 9 :element-type 'single-float))
                  (*clip-region* (cons arr NIL)))
             (declare (dynamic-extent *clip-region* arr))
             (call-next-method)
             ;; Undo current clip region if set.
             (when (cdr *clip-region*)
               (gl:stencil-op :keep :decr :decr)
               (gl:color-mask NIL NIL NIL NIL)
               (gl:depth-mask NIL)
               (let ((orig (simple:transform-matrix renderer)))
                 (setf (simple:transform-matrix renderer) (car *clip-region*))
                 (setf (simple:bounds rect) (cdr *clip-region*))
                 (render-direct rect renderer colors:purple)
                 (setf (simple:transform-matrix renderer) orig))
               (gl:stencil-op :keep :keep :keep)
               (gl:stencil-func :lequal *clip-depth* #xFF)
               (gl:color-mask T T T T)
               (gl:depth-mask T)))
           (gl:stencil-func :lequal *clip-depth* #xFF)))))

(defmethod simple:clear ((renderer renderer) extent)
  (let ((mode (simple:composite-mode renderer)))
    (setf (simple:composite-mode renderer) :clear)
    (unwind-protect
         (simple:rectangle renderer extent)
      (setf (simple:composite-mode renderer) mode))))

;; NOTE: For these to work correctly, all output colours must be premultiplied.
;;       We do this in the shaders above.
(defmethod (setf simple:composite-mode) :before (mode (renderer renderer))
  (ecase mode
    (:source-over
     (gl:blend-func-separate :one :one-minus-src-alpha :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination-over
     (gl:blend-func-separate :one-minus-dst-alpha :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:clear
     (gl:blend-func-separate :zero :zero :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:source
     (gl:blend-func-separate :one :zero :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination
     (gl:blend-func-separate :zero :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:source-in
     (gl:blend-func-separate :dst-alpha :zero :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination-in
     (gl:blend-func-separate :zero :src-alpha :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:source-out
     (gl:blend-func-separate :one-minus-dst-alpha :zero :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination-out
     (gl:blend-func-separate :zero :one-minus-src-alpha :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination-atop
     (gl:blend-func-separate :one-minus-dst-alpha :src-alpha :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:xor
     (gl:blend-func-separate :one-minus-dst-alpha :one-minus-src-alpha :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:add
     (gl:blend-func-separate :src-alpha :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:multiply
     (gl:blend-func-separate :zero :src-color :one :one-minus-src-alpha)
     (gl:blend-equation :func-add)
     (gl:blend-equation :multiply-khr))
    (:screen
     (gl:blend-func-separate :one :one-minus-src-color :one :one-minus-src-alpha)
     (gl:blend-equation :func-add)
     (gl:blend-equation :screen-khr))
    (:overlay
     (gl:blend-equation :overlay-khr))
    (:darken
     (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
     (gl:blend-equation :max)
     (gl:blend-equation :darken-khr))
    (:lighten
     (gl:blend-equation :lighten-khr))
    (:dodge
     (gl:blend-equation :colordodge-khr))
    (:burn
     (gl:blend-equation :colorburn-khr))
    (:hard-light
     (gl:blend-equation :hardlight-khr))
    (:soft-light
     (gl:blend-equation :softlight-khr))
    (:difference
     (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-subtract)
     (gl:blend-equation :difference-khr))
    (:exclusion
     (gl:blend-equation :exclusion-khr))
    (:hue
     (gl:blend-equation :hue-khr))
    (:saturation
     (gl:blend-equation :saturation-khr))
    (:color
     (gl:blend-equation :color-khr))
    (:luminosity
     (gl:blend-equation :luminosity-khr))
    (:invert
     (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-reverse-subtract))))

(defclass line-strip (simple:line-strip)
  ((data :accessor data)
   (size :initform NIL :accessor size)))

(defmethod shared-initialize :after ((shape line-strip) slots &key (points NIL points-p))
  (when points-p
    (setf (data shape) (make-line-array points))))

(defmethod simple:line-strip ((renderer renderer) (points vector) &rest initargs)
  (apply #'make-instance 'line-strip :points points initargs))

(defmethod simple:line-strip ((renderer renderer) (points cons) &rest initargs)
  (apply #'make-instance 'line-strip :points points initargs))

(defmethod render-direct ((shape line-strip) renderer color)
  (let ((shader (resource 'line-shader renderer))
        (data (data shape)))
    (update-vertex-buffer (resource 'line-vbo renderer) data)
    (bind shader)
    (setf (uniform shader "transform") (simple:transform-matrix renderer))
    (setf (uniform shader "color") color)
    (setf (uniform shader "line_width") (alloy:to-px (simple:line-width shape)))
    (setf (uniform shader "gap") (case (simple:line-style shape)
                                   (:dashed 0.3)
                                   (:dotted 1.0)
                                   (T 0.0)))
    (setf (uniform shader "view_size") (view-size renderer))
    ;; TODO: line caps, joints?
    (draw-vertex-array (resource 'line-vao renderer) :triangles 0 (min (/ (length data) 4) (* (or (size shape) 1000000) 6)))))

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

(defmethod render-direct ((shape simple:filled-rectangle) renderer color)
  (let ((shader (resource 'basic-shader renderer)))
    (bind shader)
    (simple:with-pushed-transforms (renderer)
      (simple:translate renderer (simple:bounds shape))
      (simple:scale renderer (simple:bounds shape))
      (setf (uniform shader "transform") (simple:transform-matrix renderer)))
    (setf (uniform shader "color") color)
    (setf (uniform shader "view_size") (view-size renderer))
    (draw-vertex-array (resource 'rect-fill-vao renderer) :triangles 0 6)))

(defmethod render-direct ((shape simple:outlined-rectangle) renderer color)
  (let ((shader (resource 'line-shader renderer)))
    (bind shader)
    (simple:with-pushed-transforms (renderer)
      (simple:translate renderer (simple:bounds shape))
      (simple:scale renderer (simple:bounds shape))
      (setf (uniform shader "transform") (simple:transform-matrix renderer)))
    (setf (uniform shader "color") color)
    (setf (uniform shader "line_width") (alloy:to-px (simple:line-width shape)))
    (setf (uniform shader "view_size") (view-size renderer))
    ;; TODO: rounded corners?
    (draw-vertex-array (resource 'rect-line-vao renderer) :triangles 0 24)))

(defmethod render-direct ((shape simple:filled-ellipse) renderer color)
  (let ((shader (resource 'circle-fill-shader renderer))
        (extent (alloy:ensure-extent (simple:bounds shape))))
    (bind shader)
    (simple:with-pushed-transforms (renderer)
      (let ((w (alloy:pxw extent))
            (h (alloy:pxh extent)))
        (simple:translate renderer (simple:bounds shape))
        (simple:scale renderer (alloy:px-size w h)))
      (setf (uniform shader "transform") (simple:transform-matrix renderer)))
    (setf (uniform shader "start_angle") (simple:start-angle shape))
    (setf (uniform shader "end_angle") (simple:end-angle shape))
    (setf (uniform shader "color") color)
    (setf (uniform shader "view_size") (view-size renderer))
    (draw-vertex-array (resource 'rect-fill-vao renderer) :triangles 0 6)))

(defmethod render-direct ((shape simple:outlined-ellipse) (renderer renderer) color)
  (let* ((shader (resource 'circle-line-shader renderer))
         (extent (alloy:ensure-extent (simple:bounds shape)))
         (w (alloy:pxw extent))
         (h (alloy:pxh extent)))
    (bind shader)
    (simple:with-pushed-transforms (renderer)
      (simple:translate renderer (simple:bounds shape))
      (simple:scale renderer (alloy:px-size w h))
      (setf (uniform shader "transform") (simple:transform-matrix renderer)))
    (setf (uniform shader "start_angle") (simple:start-angle shape))
    (setf (uniform shader "end_angle") (simple:end-angle shape))
    (setf (uniform shader "color") color)
    ;; KLUDGE: I don't think this is quite right yet but whatever.
    (setf (uniform shader "line_width") (/ (alloy:to-px (simple:line-width shape))
                                           (max w h)))
    (setf (uniform shader "view_size") (view-size renderer))
    (draw-vertex-array (resource 'rect-fill-vao renderer) :triangles 0 6)))

(defclass polygon (simple:polygon)
  ((data :accessor data)))

(defmethod shared-initialize :after ((shape polygon) slots &key)
  (let* ((points (simple:points shape))
         (data (make-array (* 2 (length points)) :element-type 'single-float)))
    (loop for i from 0 by 2
          for point in points
          do (setf (aref data (+ 0 i)) (alloy:pxx point))
             (setf (aref data (+ 1 i)) (alloy:pxy point)))
    (setf (data shape) data)))

(defmethod simple:polygon ((renderer renderer) points &rest initargs)
  (apply #'make-instance 'polygon :points points initargs))

(defmethod render-direct ((shape polygon) renderer color)
  (let ((shader (resource 'basic-shader renderer))
        (data (data shape)))
    (update-vertex-buffer (resource 'stream-vbo renderer) data)
    (bind shader)
    (setf (uniform shader "transform") (simple:transform-matrix renderer))
    (setf (uniform shader "color") color)
    ;; FIXME: This does not work with quite a few non-convex polygons
    (draw-vertex-array (resource 'stream-vao renderer) :triangle-fan 0 (/ (length data) 2))))

(defmethod render-direct ((shape simple:icon) renderer color)
  (let ((shader (resource 'image-shader renderer)))
    (simple:with-pushed-transforms (renderer)
      (let* ((bounds (alloy:ensure-extent (simple:bounds shape)))
             (isize (simple:resolve-scale bounds (simple:size (simple:image shape)) (simple:sizing shape)))
             (off (simple:resolve-alignment bounds :middle :middle isize)))
        (simple:clip renderer bounds)
        ;; FIXME: Dunno that alignment and sizing should be done here...
        (bind shader)
        (bind (simple:image shape))
        (setf (uniform shader "uv_scale") (alloy:ensure-extent (simple:size shape)))
        (setf (uniform shader "uv_offset") (simple:shift shape))
        (simple:translate renderer off)
        (simple:scale renderer isize))
      (setf (uniform shader "transform") (simple:transform-matrix renderer))
      (draw-vertex-array (resource 'rect-fill-vao renderer) :triangles 0 6))))

(defmethod simple:request-image ((renderer renderer) (data vector) &rest args &key size)
  (etypecase data
    ((vector (unsigned-byte 8))
     (make-texture renderer (alloy:pxw size) (alloy:pxh size) data))
    (string
     (apply #'simple:request-image renderer (pathname data) args))))
