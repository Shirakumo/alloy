#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass plot (value-component)
  ((x-range :initarg :x-range :initform T :accessor x-range)
   (y-range :initarg :y-range :initform T :accessor y-range)
   (plot-points :initform #() :accessor plot-points)))

(defmethod (setf x-range) :after (value (plot plot))
  (mark-for-render plot)
  (recompute-plot-points plot))

(defmethod (setf y-range) :after (value (plot plot))
  (mark-for-render plot)
  (recompute-plot-points plot))

(defmethod suggest-size (size (plot plot))
  (size (w size) (umax (un 400) (h size))))

(defmethod value-changed ((plot plot))
  (call-next-method)
  (recompute-plot-points plot))

(defun recompute-plot-points (plot)
  (let ((x-range (x-range plot))
        (y-range (y-range plot))
        (data (value plot))
        (extent (bounds plot))
        xmin xmax ymin ymax)
    (etypecase x-range
      (cons (setf xmin (car x-range)
                  xmax (cdr x-range)))
      ((eql T) (setf xmin 0
                     xmax (length data))))
    (etypecase y-range
      (cons (setf ymin (car y-range)
                  ymax (cdr y-range)))
      ((eql T) (setf ymin (loop for p across data minimize p)
                     ymax (loop for p across data maximize p))))
    (let* ((x-scale (/ (max 1 (pxw extent)) (- xmax xmin)))
           (y-scale (/ (max 1 (pxh extent)) (- ymax ymin)))
           (length (max (- xmax xmin)))
           (points (plot-points plot)))
      (when (/= length (length points))
        (let ((new (make-array length)))
          (replace new points)
          (loop for i from (length points) below (length new)
                do (setf (aref new i) (px-point 0 0)))
          (setf (plot-points plot) (setf points new))))
      (loop for i from 0 below (length points)
            for x from (/ x-scale 2) by x-scale
            for p = (aref data (+ i xmin))
            for y = (* (- p ymin -0.5) y-scale)
            for point = (aref points i)
            do (setf (slot-value (x point) 'value) x)
               (setf (slot-value (y point) 'value) y)))))

(defmethod render :around ((renderer renderer) (component plot))
  ;; Ensures that a plotted line doesn't leave the bounds of the plot
  (with-constrained-visibility ((bounds component) renderer)
    (call-next-method)))
