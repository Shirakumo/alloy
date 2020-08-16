#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass plot (value-component)
  ((x-range :initarg :x-range :initform T :accessor x-range)
   (y-range :initarg :y-range :initform T :accessor y-range)))

(defmethod (setf x-range) :after (value (plot plot))
  (mark-for-render plot))

(defmethod (setf y-range) :after (value (plot plot))
  (mark-for-render plot))

(defmethod suggest-bounds (extent (plot plot))
  (extent (x extent) (y extent) (w extent) (umax (un 400) (h extent))))

(defmethod render :around ((renderer renderer) (component plot))
  ;; Ensures that a plotted line doesn't leave the bounds of the plot
  (with-constrained-visibility ((bounds component) renderer)
    (call-next-method)))

(defmethod plot-points ((plot plot))
  (let ((x-range (x-range plot))
        (y-range (y-range plot))
        (data (value plot))
        (extent (bounds plot)))
    (when (eql T x-range)
      (setf x-range (cons 0 (length data))))
    (when (eql T y-range)
      (setf y-range (cons (loop for p across data minimize p)
                          (loop for p across data maximize p))))
    (destructuring-bind (xmin . xmax) x-range
      (destructuring-bind (ymin . ymax) y-range
        (let ((x-scale (/ (max 1 (pxw extent)) (- xmax xmin)))
              (y-scale (/ (max 1 (pxh extent)) (- ymax ymin)))
              (points (make-array (max 0 (- xmax xmin)))))
          (loop for i from 0 below (length points)
                for x from (/ x-scale 2) by x-scale
                for p = (aref data (+ i xmin))
                for y = (* (- p ymin -0.5) y-scale)
                do (setf (aref points i) (px-point x y)))
          points)))))
