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

(defmethod plot-points ((plot plot))
  (let ((x-range (x-range plot))
        (y-range (y-range plot))
        (data (value plot))
        (extent (bounds plot)))
    (when (eql T x-range)
      (setf x-range (list 0 (length data))))
    (when (eql T y-range)
      (setf y-range (list (max 0 (loop for p across data minimize p))
                          (loop for p across data maximize p))))
    (destructuring-bind (xmin xmax) x-range
      (destructuring-bind (ymin ymax) y-range
        (let ((x-scale (/ (max 1 (pxw extent)) (1+ (- xmax xmin))))
              (y-scale (/ (max 1 (pxh extent)) (1+ (- ymax ymin))))
              (points (make-array (max 0 (- xmax xmin)))))
          (loop for i from 0 below (length points)
                for x from (/ x-scale 2) by x-scale
                for p = (aref data (+ i xmin))
                for y = (+ (* p y-scale) (/ y-scale 2) (- ymin))
                do (setf (aref points i) (px-point x y)))
          points)))))
