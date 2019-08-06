#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass box-layout (layout)
  ((min-size :initarg :min-size :initform (size) :accessor min-size)
   (stretch :initarg :stretch :initform T :acccessor stretch)))

(defclass vertical-box-layout (box-layout)
  ())

(defmethod notice-bounds :after ((element layout-element) (layout vertical-box-layout))
  )

(defmethod suggest-bounds (extent (layout vertical-box-layout) ui)
  (destructure-extent (:x x :y y :w w) extent
    (let ((mh (vy (min-size layout))))
      (do-elements (element layout)
        (let ((bounds (suggest-bounds (extent x y w mh) element ui)))
          (incf y (extent-h bounds))
          )))))
