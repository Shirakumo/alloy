#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass fixed-layout (layout)
  ())

(defmethod notice-bounds :after ((element layout-element) (layout fixed-layout))
  ;; Calculate max bound
  (let ((extent (extent layout)))
    (destructure-extent (:x lx :y ly :w lw :h lh :to-px T) extent
      (destructure-extent (:x ex :y ey :w ew :h eh :to-px T) (extent element)
        (let ((l (min lx ex))
              (b (min ly ey))
              (r (max (+ lx lw) (+ ex ew)))
              (u (max (+ ly lh) (+ ey eh))))
          (setf lx l
                ly b
                lw (- r l)
                lh (- u b))))
      (setf (extent-x extent) (unit lx))
      (setf (extent-y extent) (unit ly))
      (setf (extent-w extent) (unit lw))
      (setf (extent-h extent) (unit lh))))
  (notice-bounds layout (layout-parent layout)))

(defmethod suggest-bounds (extent (layout fixed-layout)))

(defmethod enter ((element layout-element) (layout fixed-layout) &key (x (arg! :x)) (y (arg! :y)) (w (arg! :w)) (h (arg! :h)))
  (call-next-method)
  (setf (extent-x (extent element)) (unit x))
  (setf (extent-x (extent element)) (unit y))
  (setf (extent-x (extent element)) (unit w))
  (setf (extent-x (extent element)) (unit h))
  ;; Ensure we set the layout extent to the element bounds or we would calculate
  ;; the max bound wrong.
  (when (= 1 (element-count layout))
    (setf (bounds layout) (copy-extent (extent element)))))

(defmethod leave :after ((element layout-element) (layout fixed-layout))
  (when (= 0 (element-count layout))
    (setf (bounds layout) (extent))))

(defmethod update ((element layout-element) (layout fixed-layout) &key x y w h)
  (let ((e (extent element)))
    (when x (setf (extent-x e) (unit x)))
    (when y (setf (extent-y e) (unit y)))
    (when w (setf (extent-w e) (unit w)))
    (when h (setf (extent-h e) (unit h)))
    element))
