#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(defclass canvas-style ()
  ((pattern :initform colors:black :accessor pattern)
   (line-width :initform (alloy:un 1.0) :accessor line-width)
   (line-style :initform :solid :accessor line-style)
   (join-style :initform :none :accessor join-style)
   (cap-style :initform :none :accessor cap-style)
   (family :initform "sans-serif" :accessor family)
   (size :initform (alloy:un 12.0) :accessor size)))

(defclass canvas (alloy:component)
  ((alloy:data :initform (make-instance 'alloy::vector-data :vector (make-array 0 :adjustable T :fill-pointer T)))
   (renderer :initform NIL :initarg :renderer :accessor renderer)
   (style :initform (make-instance 'canvas-style) :accessor style)
   (location :initform (alloy:point) :accessor location)
   (state :initform NIL :accessor state)))

(defgeneric draw-shape (canvas shape &rest initargs))
(defgeneric draw-rectangle (canvas x y w h &optional corner-radii))
(defgeneric draw-ellipse (canvas x y w h &optional start-angle end-angle))
(defgeneric draw-curve (canvas x1 y1 wx1 wy1 x2 y2 wx2 wy2))
(defgeneric fill-rectangle (canvas x y w h &optional corner-radii))
(defgeneric fill-ellipse (canvas x y w h &optional start-angle end-angle))
(defgeneric draw-text (canvas x y string &optional size))
(defgeneric draw-image (canvas x y image &optional w h))
(defgeneric start-line (canvas &optional x y))
(defgeneric start-polygon (canvas &optional x y))
(defgeneric complete-shape (canvas))
(defgeneric move-to (canvas x y))
(defgeneric push-matrix (canvas))
(defgeneric pop-matrix (canvas))
(defgeneric style (canvas))
(defgeneric (setf style) (value canvas))

(macrolet ((wrap (&rest funcs)
             `(progn ,@(loop for func in funcs
                             collect `(defmethod ,func ((canvas canvas)) 
                                        (,func (style canvas)))
                             collect `(defmethod (setf ,func) (value (canvas canvas))
                                        (setf (,func (style canvas)) value))))))
  (wrap pattern line-width line-style join-style cap-style family size))

(defmethod draw-shape ((canvas canvas) function &rest initargs)
  (alloy:with-unit-parent canvas
    (let ((shape (apply function (renderer canvas) initargs)))
      (alloy::push-element (cons 'alloy:render shape) (alloy:data canvas))
      shape)))

(defmethod draw-shape ((canvas canvas) (shape shape) &rest initargs)
  (let ((shape (if initargs (apply #'reinitialize-instance shape initargs) shape)))
    (alloy::push-element (cons 'alloy:render shape) (alloy:data canvas))
    shape))

(defmethod draw-rectangle ((canvas canvas) x y w h &optional corner-radii)
  (let ((style (style canvas)))
    (draw-shape canvas 'rectangle (alloy:extent x y w h)
                :pattern (pattern style)
                :corner-radii corner-radii
                :line-width (line-width style)
                :line-style (line-style style)
                :join-style (join-style style)
                :cap-style (cap-style style))))

(defmethod draw-ellipse ((canvas canvas) x y w h &optional (start-angle 0.0) (end-angle (* 2 PI)))
  (let ((style (style canvas)))
    (draw-shape canvas 'ellipse (alloy:extent x y w h)
                :pattern (pattern style)
                :start-angle (float start-angle)
                :end-angle (float end-angle)
                :line-width (line-width style)
                :line-style (line-style style)
                :join-style (join-style style)
                :cap-style (cap-style style))))

(defmethod draw-curve ((canvas canvas) x1 y1 wx1 wy1 x2 y2 wx2 wy2)
  (let ((style (style canvas))
        (points (make-array 4)))
    (setf (aref points 0) (alloy:point x1 y1))
    (setf (aref points 1) (alloy:point wx1 wy1))
    (setf (aref points 2) (alloy:point wx2 wy2))
    (setf (aref points 3) (alloy:point x2 y2))
    (draw-shape canvas 'curve points
                :pattern (pattern style)
                :line-width (line-width style)
                :line-style (line-style style)
                :join-style (join-style style)
                :cap-style (cap-style style))))

(defmethod fill-rectangle ((canvas canvas) x y w h &optional corner-radii)
  (let ((style (style canvas)))
    (draw-shape canvas 'rectangle (alloy:extent x y w h)
                :pattern (pattern style)
                :corner-radii corner-radii)))

(defmethod fill-ellipse ((canvas canvas) x y w h &optional (start-angle 0.0) (end-angle (* 2 PI)))
  (let ((style (style canvas)))
    (draw-shape canvas 'ellipse (alloy:extent x y w h)
                :pattern (pattern style)
                :start-angle (float start-angle)
                :end-angle (float end-angle))))

(defmethod draw-text ((canvas canvas) x y string &optional size)
  (let ((style (style canvas)))
    (draw-shape canvas 'text (alloy:extent x y MOST-POSITIVE-SINGLE-FLOAT (size style)) string
                :pattern (pattern style)
                :size (or size (size style))
                :font (family style))))

(defmethod draw-image ((canvas canvas) x y image &optional w h)
  (let ((image (request-image (renderer canvas) image)))
    (draw-shape canvas 'icon (alloy:extent x y (or w (alloy:w image)) (or h (alloy:h image))) image)))

(defmethod start-line ((canvas canvas) &optional x y)
  (when (state canvas)
    (complete-shape canvas))
  (let ((points (make-array 0 :adjustable T :fill-pointer T)))
    (vector-push-extend (if x (alloy:point x y) (location canvas)) points)
    (setf (state canvas) (cons :line points))))

(defmethod start-polygon ((canvas canvas) &optional x y)
  (when (state canvas)
    (complete-shape canvas))
  (let ((points (make-array 0 :adjustable T :fill-pointer T)))
    (vector-push-extend (if x (alloy:point x y) (location canvas)) points)
    (setf (state canvas) (cons :polygon points))))

(defmethod complete-shape ((canvas canvas))
  (let ((style (style canvas)))
    (case (car (state canvas))
      ((NIL))
      (:line
       (draw-shape canvas 'line-strip (cdr (state canvas))
                   :line-width (line-width style)
                   :line-style (line-style style)
                   :join-style (join-style style)
                   :cap-style (cap-style style)))
      (:polygon
       (draw-shape canvas 'polygon (cdr (state canvas))
                   :pattern (pattern style))))))

(defmethod move-to ((canvas canvas) x y)
  (setf (location canvas) (alloy:point x y))
  (when (state canvas)
    (vector-push-extend (location canvas) (cdr (state canvas)))))

(defmethod clip ((canvas canvas) extent)
  (alloy::push-element (cons 'clip extent) (alloy:data canvas)))

(defmethod translate ((canvas canvas) point)
  (alloy::push-element (cons 'translate point) (alloy:data canvas)))

(defmethod scale ((canvas canvas) size)
  (alloy::push-element (cons 'scale size) (alloy:data canvas)))

(defmethod rotate ((canvas canvas) phi)
  (alloy::push-element (cons 'rotate phi) (alloy:data canvas)))

(defmethod clear ((canvas canvas) bounds)
  (alloy::push-element (cons 'clear bounds) (alloy:data canvas)))

(defmethod (setf composite-mode) (mode (canvas canvas))
  (alloy::push-element (cons 'composite-mode mode) (alloy:data canvas)))

(defmethod push-matrix ((canvas canvas))
  (alloy::push-element (cons 'push-matrix NIL) (alloy:data canvas)))

(defmethod pop-matrix ((canvas canvas))
  (alloy::push-element (cons 'pop-matrix NIL) (alloy:data canvas)))

(defmethod alloy:render ((renderer renderer) (canvas canvas))
  (let ((matrix-stack ()))
    (loop for (action . arg) across (alloy:value (alloy:data canvas))
          do (ecase action
               (alloy:render 
                (alloy:render renderer arg))
               (clip
                (clip renderer arg))
               (translate
                (translate renderer arg))
               (scale
                (scale renderer arg))
               (rotate
                (rotate renderer arg))
               (clear
                (clear renderer arg))
               (composite-mode
                (setf (composite-mode renderer) arg))
               (push-matrix
                (push (make-array 9 :element-type 'single-float :initial-contents (transform-matrix renderer)) matrix-stack))
               (pop-matrix
                (let ((prev (pop matrix-stack)))
                  (when prev
                    (replace (transform-matrix renderer) prev))))))))
