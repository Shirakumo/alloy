#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.opengl)

(defvar *shaders-directory*
  #.(merge-pathnames "shaders/" (or *compile-file-pathname* *load-pathname*)))
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

(defun intersect (o1x o1y d1x d1y o2x o2y d2x d2y)
  (declare (type single-float o1x o1y d1x d1y o2x o2y d2x d2y))
  (let ((u (/ (- (* d2x (- o2y o1y)) (* d2y (- o2x o1x)))
              (- (* d2x d1y) (* d2y d1x)))))
    (values (+ o1x (* d1x u))
            (+ o1y (* d1y u)))))

(defun make-shader-from-file (renderer name &optional (file (make-pathname :name (string-downcase name) :type "glsl" :defaults *shaders-directory*)))
  (unless (resource name renderer NIL)
    (let* ((contents (alexandria:read-file-into-string file))
           (vert-start (search "//VERT" contents))
           (frag-start (search "//FRAG" contents))
           (vert (subseq contents vert-start (if (< vert-start frag-start) frag-start (length contents))))
           (frag (subseq contents frag-start (if (< frag-start vert-start) vert-start (length contents)))))
      (gl-extension-case
        (:GL-KHR-BLEND-EQUATION-ADVANCED
         (setf frag (concatenate 'string (alexandria:read-file-into-string (make-pathname :name "blend-equation" :type "glsl" :defaults *shaders-directory*))
                                 (string #\Linefeed)
                                 frag))))
      (setf (resource name renderer) (make-shader renderer :vertex-shader vert :fragment-shader frag)))))

(defun make-line-array (points width cap-style join-style &key closed)
  (let ((array (make-array (max 0 (* 6 5 (1- (length points)))) :element-type 'single-float :adjustable T :fill-pointer 0))
        (dots (make-array 0 :element-type 'single-float :adjustable T :fill-pointer 0))
        (w (* 0.5 (alloy:to-px width)))
        (tt 0.0) pax pay pbx pby pux puy)
    (labels ((vertex (x y nx ny tt)
               (vector-push-extend x array)
               (vector-push-extend y array)
               (vector-push-extend nx array)
               (vector-push-extend ny array)
               (vector-push-extend tt array))
             (dvertex (x y u v)
               (vector-push-extend x dots)
               (vector-push-extend y dots)
               (vector-push-extend u dots)
               (vector-push-extend v dots))
             (join (ax ay bx by ux uy tt0)
               (let* ((cw (if (< 0 (- (* (- ax pax) (- by pay))
                                      (* (- bx pax) (- ay pay))))
                              -1 +1))
                      (o2x (+ ax (* w cw ux))) (o2y (+ ay (* w cw uy)))
                      (o1x (+ pbx (* w cw pux))) (o1y (+ pby (* w cw puy))))
                 (case join-style
                   (:bevel
                    (vertex o1x o1y (* cw pux) (* cw puy) tt0)
                    (vertex ax ay 0.0 0.0 tt0)
                    (vertex o2x o2y (* cw ux) (* cw uy) tt0))
                   (:miter
                    (let ((d1x (- pbx pax)) (d1y (- pby pay))
                          (d2x (- bx ax)) (d2y (- by ay)))
                      (multiple-value-bind (ex ey) (intersect o1x o1y d1x d1y o2x o2y d2x d2y)
                        (vertex o1x o1y (+ pux) (+ puy) tt0)
                        (vertex ax ay 0.0 0.0 tt0)
                        (vertex ex ey (+ pux) (+ puy) tt0)
                        (vertex ex ey (+ ux) (+ uy) tt0)
                        (vertex ax ay 0.0 0.0 tt0)
                        (vertex o2x o2y (+ ux) (+ uy) tt0))))
                   (:round
                    (let ((r (max (sqrt (+ (expt (- ax o1x) 2) (expt (- ay o1y) 2)))
                                  (sqrt (+ (expt (- ax o2x) 2) (expt (- ay o2y) 2))))))
                      (dvertex (- ax r) (- ay r) 0.0 0.0)
                      (dvertex (+ ax r) (- ay r) 1.0 0.0)
                      (dvertex (+ ax r) (+ ay r) 1.0 1.0)
                      (dvertex (+ ax r) (+ ay r) 1.0 1.0)
                      (dvertex (- ax r) (+ ay r) 0.0 1.0)
                      (dvertex (- ax r) (- ay r) 0.0 0.0))))))
             (cap (bx by ux uy tt)
               (case cap-style
                 (:square
                  (let ((ax bx) (ay by)
                        (bx (+ bx (* uy w)))
                        (by (+ by (* (- ux) w))))
                    (vertex (- ax (* w ux)) (- ay (* w uy)) (- ux) (- uy) tt)
                    (vertex (- bx (* w ux)) (- by (* w uy)) (- ux) (- uy) tt)
                    (vertex (+ ax (* w ux)) (+ ay (* w uy)) (+ ux) (+ uy) tt)
                    (vertex (- bx (* w ux)) (- by (* w uy)) (- ux) (- uy) tt)
                    (vertex (+ bx (* w ux)) (+ by (* w uy)) (+ ux) (+ uy) tt)
                    (vertex (+ ax (* w ux)) (+ ay (* w uy)) (+ ux) (+ uy) tt)))
                 (:spike
                  (let ((ax (- bx (* uy (* 0.5 w))))
                        (ay (- by (* (- ux) (* 0.5 w))))
                        (bx (+ bx (* uy (* 2 w))))
                        (by (+ by (* (- ux) (* 2 w)))))
                    (vertex (- ax (* w ux)) (- ay (* w uy)) (- ux) (- uy) tt)
                    (vertex bx by (- ux) (- uy) tt)
                    (vertex ax ay (+ ux) (+ uy) tt)
                    (vertex (+ ax (* w ux)) (+ ay (* w uy)) (+ ux) (+ uy) tt)
                    (vertex ax ay (+ ux) (+ uy) tt)
                    (vertex bx by (- ux) (- uy) tt)))
                 (:round
                  (dvertex (- bx w) (- by w) 0.0 0.0)
                  (dvertex (+ bx w) (- by w) 1.0 0.0)
                  (dvertex (+ bx w) (+ by w) 1.0 1.0)
                  (dvertex (+ bx w) (+ by w) 1.0 1.0)
                  (dvertex (- bx w) (+ by w) 0.0 1.0)
                  (dvertex (- bx w) (- by w) 0.0 0.0))))
             (line (a b)
               (let* ((ax (alloy:pxx a)) (ay (alloy:pxy a))
                      (bx (alloy:pxx b)) (by (alloy:pxy b))
                      (ux (- (- by ay))) (uy (- bx ax))
                      (len (sqrt (+ (* ux ux) (* uy uy))))
                      (tt0 tt) (tt1 (+ tt len)))
                 (when (< 0 len)
                   (setf ux (/ ux len))
                   (setf uy (/ uy len))
                   (vertex (- ax (* w ux)) (- ay (* w uy)) (- ux) (- uy) tt0)
                   (vertex (- bx (* w ux)) (- by (* w uy)) (- ux) (- uy) tt1)
                   (vertex (+ ax (* w ux)) (+ ay (* w uy)) (+ ux) (+ uy) tt0)
                   (vertex (- bx (* w ux)) (- by (* w uy)) (- ux) (- uy) tt1)
                   (vertex (+ bx (* w ux)) (+ by (* w uy)) (+ ux) (+ uy) tt1)
                   (vertex (+ ax (* w ux)) (+ ay (* w uy)) (+ ux) (+ uy) tt0)
                   (cond (pax
                          (join ax ay bx by ux uy tt0))
                         ((not closed)
                          (cap ax ay (- ux) (- uy) tt0)))
                   (setf tt tt1)
                   (setf pax ax pay ay pbx bx pby by pux ux puy uy))))
             (finish (a b)
               (if closed
                   (let* ((ax (alloy:pxx a)) (ay (alloy:pxy a))
                          (bx (alloy:pxx b)) (by (alloy:pxy b))
                          (ux (- (- by ay))) (uy (- bx ax))
                          (len (sqrt (+ (* ux ux) (* uy uy)))))
                     (setf ux (/ ux len))
                     (setf uy (/ uy len))
                     (join ax ay bx by ux uy tt))
                   (cap pbx pby pux puy tt))))
      (etypecase points
        (list
         (loop for (a b) on points
               while b do (line a b))
         (when pax
           (finish (first points) (second points))))
        (vector
         (loop for i from 0 below (1- (length points))
               do (line (aref points i) (aref points (1+ i))))
         (when pax
           (finish (aref points 0) (aref points 1)))))
      (values (make-array (length array) :element-type 'single-float :initial-contents array)
              (make-array (length dots) :element-type 'single-float :initial-contents dots)))))

(defmethod alloy:allocate :before ((renderer renderer))
  ;; TODO: Implement sharing between renderers.
  ;; Allocate the necessary geometry.
  (unless *gl-extensions*
    (cache-gl-extensions))
  (flet ((arr (&rest data)
           (make-array (length data) :element-type 'single-float :initial-contents data))
         (make-geometry (vbo vao content &key (data-usage :static-draw) (bindings `((:size 2 :offset 0 :stride 8))))
           (unless (resource vbo renderer NIL)
             (setf (resource vbo renderer) (make-vertex-buffer renderer content :data-usage data-usage)))
           (unless (resource vao renderer NIL)
             (setf (resource vao renderer) (make-vertex-array renderer (loop for binding in bindings
                                                                             collect (list* (resource vbo renderer) binding)))))))
    (make-geometry 'line-vbo 'line-vao (arr)
                   :data-usage :stream-draw
                   :bindings '((:size 2 :offset 0 :stride 20)
                               (:size 2 :offset 8 :stride 20)
                               (:size 1 :offset 16 :stride 20)))
    (make-geometry 'points-vbo 'points-vao (arr)
                   :data-usage :stream-draw
                   :bindings '((:size 2 :offset 0 :stride 16)
                               (:size 2 :offset 8 :stride 16)))
    (make-geometry 'rect-fill-vbo 'rect-fill-vao
                   (arr 0f0 0f0  1f0 1f0  0f0 1f0
                        0f0 0f0  1f0 0f0  1f0 1f0))
    (make-geometry 'rect-inner-vbo 'rect-inner-vao
                   ;;    W   H   XR  YR
                   (arr 0f0 0f0 0f0 4f0  0f0 0f0 4f0 4f0  0f0 1f0 1f0 1f0 ; L
                        0f0 1f0 1f0 1f0  0f0 1f0 0f0 1f0  0f0 0f0 0f0 4f0
                        0f0 0f0 4f0 0f0  1f0 0f0 3f0 0f0  0f0 0f0 4f0 4f0 ; B
                        0f0 0f0 4f0 4f0  1f0 0f0 3f0 0f0  1f0 0f0 3f0 3f0
                        0f0 0f0 4f0 4f0  1f0 0f0 3f0 3f0  0f0 1f0 1f0 1f0 ; C
                        0f0 1f0 1f0 1f0  1f0 0f0 3f0 3f0  1f0 1f0 2f0 2f0
                        1f0 1f0 2f0 2f0  1f0 0f0 3f0 3f0  1f0 1f0 0f0 2f0 ; R
                        1f0 1f0 0f0 2f0  1f0 0f0 3f0 3f0  1f0 0f0 0f0 3f0
                        0f0 1f0 1f0 1f0  1f0 1f0 2f0 2f0  1f0 1f0 2f0 0f0 ; T
                        1f0 1f0 2f0 0f0  0f0 1f0 1f0 0f0  0f0 1f0 1f0 1f0)
                   :bindings '((:size 2 :offset 0 :stride 16)
                               (:size 2 :offset 8 :stride 16)))
    (make-geometry 'rect-corner-vbo 'rect-corner-vao
                   (arr 0f0 0f0 0f0 0f0  0f0 0f0 4f0 0f0  0f0 0f0 4f0 4f0 ; BL
                        0f0 0f0 4f0 4f0  0f0 0f0 0f0 4f0  0f0 0f0 0f0 0f0
                        1f0 0f0 0f0 0f0  1f0 0f0 3f0 0f0  1f0 0f0 3f0 3f0 ; BR
                        1f0 0f0 3f0 3f0  1f0 0f0 0f0 3f0  1f0 0f0 0f0 0f0
                        1f0 1f0 0f0 0f0  1f0 1f0 2f0 0f0  1f0 1f0 2f0 2f0 ; TR
                        1f0 1f0 2f0 2f0  1f0 1f0 0f0 2f0  1f0 1f0 0f0 0f0
                        0f0 1f0 0f0 0f0  0f0 1f0 1f0 0f0  0f0 1f0 1f0 1f0 ; TL
                        0f0 1f0 1f0 1f0  0f0 1f0 0f0 1f0  0f0 1f0 0f0 0f0)
                   :bindings '((:size 2 :offset 0 :stride 16)
                               (:size 2 :offset 8 :stride 16)))
    (make-geometry 'stream-vbo 'stream-vao (arr)
                   :data-usage :stream-draw)
    (make-geometry 'gradient-vbo 'gradient-vao (arr)
                   :data-usage :stream-draw
                   :bindings '((:size 2 :offset 0 :stride 24) (:size 4 :offset 8 :stride 24))))

  ;; Allocate the necessary shaders.
  (make-shader-from-file renderer 'line-shader)
  (make-shader-from-file renderer 'points-shader)
  (make-shader-from-file renderer 'pie-fill-shader)
  (make-shader-from-file renderer 'circle-fill-shader)
  (make-shader-from-file renderer 'circle-line-shader)
  (make-shader-from-file renderer 'gradient-shader)
  (make-shader-from-file renderer 'corner-shader)
  (make-shader-from-file renderer 'rect-shader)
  (make-shader-from-file renderer 'basic-shader)
  (make-shader-from-file renderer 'image-shader))

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

(defmethod unclip ((renderer renderer) (shape simple:shape))
  (unless (cdr *clip-region*)
    (error "Clipping not applied."))
  (gl:stencil-op :keep :decr :decr)
  (gl:color-mask NIL NIL NIL NIL)
  (gl:depth-mask NIL)
  (setf (cdr *clip-region*) (simple:bounds shape))
  (replace (car *clip-region*) (simple:transform-matrix renderer))
  (render-direct shape renderer colors:black)
  (decf *clip-depth* 1)
  (setf (cdr *clip-region*) NIL)
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
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :multiply-khr))
       (T
        (gl:blend-func-separate :zero :src-color :one :one-minus-src-alpha)
        (gl:blend-equation :func-add))))
    (:screen
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :screen-khr))
       (T
        (gl:blend-func-separate :one :one-minus-src-color :one :one-minus-src-alpha)
        (gl:blend-equation :func-add))))
    (:overlay
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :overlay-khr))))
    (:darken
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :darken-khr))
       (T
        (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
        (gl:blend-equation :max))))
    (:lighten
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :lighten-khr))))
    (:dodge
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :colordodge-khr))))
    (:burn
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :colorburn-khr))))
    (:hard-light
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :hardlight-khr))))
    (:soft-light
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :softlight-khr))))
    (:difference
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :difference-khr))
       (T
        (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
        (gl:blend-equation :func-subtract))))
    (:exclusion
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :exclusion-khr))))
    (:hue
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :hue-khr))))
    (:saturation
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :saturation-khr))))
    (:color
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :color-khr))))
    (:luminosity
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :luminosity-khr))))
    (:invert
     (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-reverse-subtract))))

(defclass line-strip (simple:line-strip)
  ((line-data :accessor line-data)
   (point-data :accessor point-data)
   (size :initform NIL :accessor size)))

(defmethod shared-initialize :after ((shape line-strip) slots &key closed)
  (multiple-value-bind (lines points) (make-line-array (simple:points shape) (simple:line-width shape)
                                                       (simple:cap-style shape) (simple:join-style shape)
                                                       :closed closed)
    (setf (line-data shape) lines)
    (setf (point-data shape) points)))

(defmethod simple:line-strip ((renderer renderer) (points vector) &rest initargs)
  (apply #'make-instance 'line-strip :points points initargs))

(defmethod simple:line-strip ((renderer renderer) (points cons) &rest initargs)
  (apply #'make-instance 'line-strip :points points initargs))

(defmethod render-direct ((shape line-strip) renderer color)
  (let ((shader (resource 'line-shader renderer))
        (lines (line-data shape))
        (points (point-data shape)))
    (flet ((prepare (shader)
             (bind shader)
             (setf (uniform shader "transform") (simple:transform-matrix renderer))
             (setf (uniform shader "color") color)))
      (update-vertex-buffer (resource 'line-vbo renderer) lines)
      (prepare shader)
      (setf (uniform shader "line_width") (alloy:to-px (simple:line-width shape)))
      (setf (uniform shader "gap") (case (simple:line-style shape)
                                     (:dashed 0.3)
                                     (:dotted 1.0)
                                     (T 0.0)))
      (setf (uniform shader "view_size") (view-size renderer))
      (draw-vertex-array (resource 'line-vao renderer) :triangles 0 (floor (length lines) 5))
      (when (< 0 (length points))
        (update-vertex-buffer (resource 'points-vbo renderer) points)
        (prepare (resource 'points-shader renderer))
        (draw-vertex-array (resource 'points-vao renderer) :triangles 0 (floor (length points) 4))))))

(defclass curve (line-strip)
  ())

(defmethod shared-initialize :around ((shape curve) slots &rest initargs &key points (segments 20))
  ;; TODO: Do this GPU-side with tesselation. Requires GL4.1 though....
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
      (apply #'call-next-method shape slots :points lines :join-style :bevel initargs))))

(defmethod simple:curve ((renderer renderer) (points vector) &rest initargs)
  (apply #'make-instance 'curve :points points initargs))

(defmethod simple:curve ((renderer renderer) (points cons) &rest initargs)
  (apply #'make-instance 'curve :points points initargs))

(defmethod render-direct ((shape simple:filled-rectangle) renderer color)
  (let* ((corner-radii (simple:corner-radii shape))
         (round-p (not (every #'zerop corner-radii)))
         (extent (alloy:ensure-extent (simple:bounds shape))))
    (flet ((prepare (shader)
             (bind shader)
             (simple:with-pushed-transforms (renderer)
               (simple:translate renderer extent)
               (setf (uniform shader "transform") (simple:transform-matrix renderer)))
             (setf (uniform shader "size") (alloy:size (alloy:w extent) (alloy:h extent)))
             (setf (uniform shader "corner_radius[0]") 0.0)
             (setf (uniform shader "corner_radius[1]") (aref corner-radii 0))
             (setf (uniform shader "corner_radius[2]") (aref corner-radii 1))
             (setf (uniform shader "corner_radius[3]") (aref corner-radii 2))
             (setf (uniform shader "corner_radius[4]") (aref corner-radii 3))
             (setf (uniform shader "color") color)
             (setf (uniform shader "view_size") (view-size renderer))))
      (prepare (resource 'rect-shader renderer))
      (draw-vertex-array (resource 'rect-inner-vao renderer) :triangles 0 32)
      (when round-p
        (prepare (resource 'corner-shader renderer))
        (draw-vertex-array (resource 'rect-corner-vao renderer) :triangles 0 32)))))

(defclass outlined-rectangle (simple:outlined-rectangle line-strip)
  ())

(defmethod shared-initialize :around ((rectangle outlined-rectangle) slots &rest args &key bounds)
  (let* ((points (make-array 5))
         (bounds (alloy:ensure-extent (or bounds (simple:bounds rectangle))))
         (x (alloy:pxx bounds))
         (y (alloy:pxy bounds))
         (w (alloy:pxw bounds))
         (h (alloy:pxh bounds)))
    (setf (aref points 0) (alloy:px-point (+ x 0) (+ y 0)))
    (setf (aref points 1) (alloy:px-point (+ x w) (+ y 0)))
    (setf (aref points 2) (alloy:px-point (+ x w) (+ y h)))
    (setf (aref points 3) (alloy:px-point (+ x 0) (+ y h)))
    (setf (aref points 4) (alloy:px-point (+ x 0) (+ y 0)))
    (setf (simple:points rectangle) points)
    (apply #'call-next-method rectangle slots :closed T args)))

(defmethod simple:rectangle ((renderer renderer) bounds &rest initargs &key line-width)
  (apply #'make-instance (if line-width 'outlined-rectangle 'simple:filled-rectangle) :bounds bounds initargs))

(defmethod render-direct ((shape simple:filled-ellipse) renderer color)
  (let ((shader (if (<= (* 2 PI) (abs (- (simple:end-angle shape) (simple:start-angle shape))))
                    (resource 'circle-fill-shader renderer)
                    (resource 'pie-fill-shader renderer)))
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
    (setf (uniform shader "gap") (case (simple:line-style shape)
                                   (:dashed 0.1)
                                   (:dotted 0.3)
                                   (T 0.0)))
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
