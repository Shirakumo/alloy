#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass radio (value-component)
  ((active-value :initarg :active-value :initform (arg! :active-value) :accessor active-value)))

(defmethod activate :after ((radio radio))
  (setf (value radio) (active-value radio)))

(defmethod active-p ((radio radio))
  (eq (active-value radio) (value radio)))

