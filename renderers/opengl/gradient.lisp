#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.opengl)

(defclass gradient (simple:gradient)
  ((data :reader data)))

(defmethod shared-initialize :after ((gradient gradient) slots &key)
  (setf (slot-value gradient 'data) (compute-gradient-data gradient)))

(defmethod simple:request-gradient ((renderer renderer) type start stop stops &key)
  (make-instance (find-symbol (string type) #.*package*) :start start :stop stop :stops stops))

(defmethod render-direct ((shape gradient) renderer color)
  (let ((shader (resource 'gradient-shader renderer))
        (data (data shape)))
    (update-vertex-buffer (resource 'gradient-vbo renderer) data)
    (bind shader)
    (setf (uniform shader "transform") (simple:transform-matrix renderer))
    (draw-vertex-array (resource 'gradient-vao renderer) :triangle-strip (/ (length data) 6))))

(defmethod alloy:render :around ((renderer renderer) (shape simple:patterned-shape))
  (let ((pattern (simple:pattern shape)))
    (etypecase pattern
      (colored:color
       (call-next-method))
      (simple:image
       ;; FIXME: do this
       (simple:clip renderer shape))
      (gradient
       (simple:clip renderer shape)
       (render-direct pattern renderer colors:black)))))

(defgeneric compute-gradient-data (gradient))

;; FIXME: gradients
(defclass linear-gradient (gradient) ())

(defmethod compute-gradient-data ((gradient linear-gradient))
  (let* ((array (make-array (* 6 2 (+ 2 (length (simple:stops gradient)))) :element-type 'single-float))
         (stops (simple:stops gradient))
         (ax (alloy:pxx (simple:start gradient)))
         (ay (alloy:pxy (simple:start gradient)))
         (bx (alloy:pxx (simple:stop gradient)))
         (by (alloy:pxy (simple:stop gradient)))
         (dx (- bx ax))
         (dy (- by ay))
         ;; KLUDGE: Don't know how to stretch it to infinity.
         (ex (* 10000.0 (- dy)))
         (ey (* 10000.0 dx))
         (i -1))
    (labels ((vertex (x y c)
               (setf (aref array (incf i)) x)
               (setf (aref array (incf i)) y)
               (setf (aref array (incf i)) (colored:r c))
               (setf (aref array (incf i)) (colored:g c))
               (setf (aref array (incf i)) (colored:b c))
               (setf (aref array (incf i)) (colored:a c)))
             (stop (tt color)
               (let ((tcx (+ ax (* dx tt)))
                     (tcy (+ ay (* dy tt))))
                 (vertex (- tcx ex) (- tcy ey) color)
                 (vertex (+ tcx ex) (+ tcy ey) color))))
      (stop -1000.0 (second (aref stops 0)))
      (loop for (tt color) across stops
            do (stop tt color))
      (stop +1000.0 (second (aref stops (1- (length stops)))))
      array)))

(defclass radial-gradient (gradient)
  ((resolution :initform (* 4 16) :accessor resolution)))

(defmethod compute-gradient-data ((gradient radial-gradient))
  (let* ((res (resolution gradient))
         (array (make-array (* 6 3 res (1- (length (simple:stops gradient))))))
         (stops (simple:stops gradient))
         (ax (alloy:pxx (simple:start gradient)))
         (ay (alloy:pxy (simple:start gradient)))
         (bx (alloy:pxx (simple:stop gradient)))
         (by (alloy:pxy (simple:stop gradient)))
         (dx (- bx ax))
         (dy (- by ay))
         (r (sqrt (+ (expt dx 2) (expt dy 2))))
         (i -1)
         (step (/ (* 2 PI) res)))
    (labels ((vertex (x y c)
               (setf (aref array (incf i)) x)
               (setf (aref array (incf i)) y)
               (setf (aref array (incf i)) (colored:r c))
               (setf (aref array (incf i)) (colored:g c))
               (setf (aref array (incf i)) (colored:b c))
               (setf (aref array (incf i)) (colored:a c)))
             (stop (ft fcolor tt tcolor)
               (let ((fr (* ft r))
                     (tr (* tt r)))
                 (loop for fphi = 0 then tphi
                       for tphi from step to (* 2 PI) by step
                       for fx = (cos fphi)
                       for fy = (sin fphi)
                       for tx = (cos tphi)
                       for ty = (sin tphi)
                       do (vertex (* fr fx) (* fr fy) fcolor)
                          (vertex (* fr tx) (* fr ty) tcolor)
                          (vertex (* tr tx) (* tr ty) tcolor)
                          (vertex (* tr tx) (* tr ty) tcolor)
                          (vertex (* tr fx) (* tr fy) fcolor)
                          (vertex (* fr fx) (* fr fy) fcolor)))))
      (loop for i from 1 below (length stops)
            for (ft fcolor) = (aref stops (1- i))
            for (tt tcolor) = (aref stops i)
            do (stop ft fcolor tt tcolor))
      array)))

(defclass angle-gradient (gradient) ())

(defmethod compute-gradient-data ((gradient angle-gradient))
  (let* ((res (* 4 16))
         (array (make-array (* 6 3 res)))
         (stops (simple:stops gradient))
         (ax (alloy:pxx (simple:start gradient)))
         (ay (alloy:pxy (simple:start gradient)))
         (bx (alloy:pxx (simple:stop gradient)))
         (by (alloy:pxy (simple:stop gradient)))
         (dx (- bx ax))
         (dy (- by ay))
         (r (sqrt (+ (expt dx 2) (expt dy 2))))
         (i -1))
    (labels ((vertex (x y c)
               (setf (aref array (incf i)) x)
               (setf (aref array (incf i)) y)
               (setf (aref array (incf i)) (colored:r c))
               (setf (aref array (incf i)) (colored:g c))
               (setf (aref array (incf i)) (colored:b c))
               (setf (aref array (incf i)) (colored:a c)))
             (stop (ft fcolor tt tcolor)
               ;; FIXME: This is not correct at all, lol
               (vertex ax ay fcolor)
               (vertex (* r (sin (* 2 PI tt))) (* r (cos (* 2 PI tt))) tcolor)
               (vertex (* r (sin (* 2 PI ft))) (* r (cos (* 2 PI ft))) fcolor)))
      (loop for i from 1 below (length stops)
            for (ft fcolor) = (aref stops (1- i))
            for (tt tcolor) = (aref stops i)
            do (stop ft fcolor tt tcolor))
      array)))

;; This is the same as the radial gradient, just with a minimal resolution.
(defclass diamond-gradient (radial-gradient)
  ((resolution :initform 4)))
