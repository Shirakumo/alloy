#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass fixed-entry (layout-entry)
  ((extent :initarg :extent :initform (error "EXTENT required."))))

(defclass fixed-layout (layout)
  ())

(defmethod notice-extent :after ((element layout-element) (layout fixed-layout))
  ;; Calculate max bound
  (with-extent (:x lx :y ly :w lw :h lh) (extent layout)
    (with-extent (:x ex :y ey :w ew :h eh) (extent element)
      (let ((l (min lx ex))
            (b (min ly ey))
            (r (max (+ lx lw) (+ ex ew)))
            (u (max (+ ly lh) (+ ey eh))))
        (setf lx l ly b lw
              lw (- r l) lh (- u b)))))
  (notice-extent layout (parent layout)))

(defmethod enter ((component component) (layout fixed-layout) &key x y w h)
  (unless (and x y w h)
    (error "Must specify the X Y W H bounds."))
  (make-instance 'fixed-entry :component component :parent layout :extent
                 (extent x y w h)))

(defmethod enter :before ((element layout-element) (layout fixed-layout) &key)
  ;; Ensure we set the layout extent to the element bounds or we would calculate
  ;; the max bound wrong.
  (when (= 0 (element-count layout))
    (setf (bounds layout) (copy-extent (extent element)))))

(defmethod leave :after ((element layout-element) (layout fixed-layout))
  (when (= 0 (element-count layout))
    (setf (bounds layout) (extent 0 0 0 0))))

(defmethod update ((element fixed-entry) (layout fixed-layout) &key x y w h)
  (let ((e (extent element)))
    (when x (setf (extent-x e) x))
    (when y (setf (extent-y e) y))
    (when w (setf (extent-w e) w))
    (when h (setf (extent-h e) h))
    element))
