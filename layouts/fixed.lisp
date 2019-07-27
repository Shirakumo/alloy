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

(defmethod notice-extent ((element fixed-entry) (layout fixed-layout))
  (setf (extent (component element)) (extent element)))

(defmethod enter ((component component) (layout fixed-layout) &key x y w h)
  (make-instance 'fixed-entry :component component :porent layout :extent
                 (make-extent x y w h)))

(defmethod update ((element fixed-entry) (layout fixed-layout) &key x y w h)
  (let ((e (extent element)))
    (when x (setf (extent-x e) x))
    (when y (setf (extent-y e) y))
    (when w (setf (extent-w e) w))
    (when h (setf (extent-h e) h))
    element))
