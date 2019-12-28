#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass icon (component)
  ())

(defclass value-component (component)
  ())

(defmethod initialize-instance :after ((component value-component) &key)
  (on (setf value) (value (data component))
    (declare (ignore value))
    (mark-for-render component)))

(defmethod value ((component value-component))
  (value (data component)))

(defmethod (setf value) (new-value (component value-component))
  (setf (value (data component)) new-value))

(defmethod refresh ((component value-component))
  (setf (value component) (value component)))

(defclass direct-value-component (value-component)
  ((value :initarg :value :accessor value)
   (data :initform ())))

(defmethod initialize-instance ((component direct-value-component) &key data)
  (when data (error "DATA is not allowed for a ~s" (type-of component)))
  (call-next-method)
  (setf (slot-value component 'data) component))

(defclass progress (value-component)
  ((maximum :initarg :maximum :initform 100 :accessor maximum)))

(defclass label (value-component)
  ())

(defmethod component-class-for-object ((string string))
  (find-class 'label))

(defclass label* (label direct-value-component) ())

(defmethod enter ((string string) (layout layout) &rest args)
  (apply #'enter (make-instance 'label* :value string) layout args))
