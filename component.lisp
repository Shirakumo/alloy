#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass component (renderable)
  ())

(defmethod handle ((event event) (component component) ctx)
  (decline))

(defmethod suggest-bounds (extent (component component))
  extent)

(defmethod activate ((component component)))

(defmethod exit ((component component)))
