(in-package #:org.shirakumo.alloy)

(defclass radio (value-component)
  ((active-value :initarg :active-value :initform (arg! :active-value) :accessor active-value)
   (sizing-strategy :initform (fixed-size 15))))

(defmethod activate :after ((radio radio))
  (setf (value radio) (active-value radio)))

(defmethod active-p ((radio radio))
  (eq (active-value radio) (value radio)))

