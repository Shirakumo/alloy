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
  (let ((extent (bounds layout)))
    (destructure-extent (:x lx :y ly :w lw :h lh :to-px T) extent
      (destructure-extent (:x ex :y ey :w ew :h eh :to-px T) (bounds element)
        (let ((l (min lx ex))
              (b (min ly ey))
              (r (max (+ lx lw) (+ ex ew)))
              (u (max (+ ly lh) (+ ey eh))))
          (setf (bounds layout)
                (px-extent l b (- r l) (- u b)))))))
  (notice-bounds layout (layout-parent layout)))

(defmethod suggest-bounds (extent (layout fixed-layout)))

(defmethod enter ((element layout-element) (layout fixed-layout) &key (x (arg! :x)) (y (arg! :y)) (w (arg! :w)) (h (arg! :h)))
  (call-next-method)
  (setf (bounds element) (extent x y w h))
  ;; Ensure we set the layout extent to the element bounds or we would calculate
  ;; the max bound wrong.
  (when (= 1 (element-count layout))
    (setf (bounds layout) (copy-extent (extent element)))))

(defmethod leave :after ((element layout-element) (layout fixed-layout))
  (when (= 0 (element-count layout))
    (setf (bounds layout) (extent))))

(defmethod update ((element layout-element) (layout fixed-layout) &key x y w h)
  (let ((e (bounds element)))
    (setf (bounds element)
          (extent (or x (extent-x e))
                  (or y (extent-y e))
                  (or w (extent-w e))
                  (or h (extent-h e))))
    element))
