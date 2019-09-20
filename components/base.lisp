#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass label (component)
  ())

(defmethod enter ((string string) (layout layout) &rest args)
  (apply #'update (represent-with 'label string layout NIL) layout args))

(defmethod component-class-for-object ((string string))
  (find-class 'label))

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
  ((value :initarg :value :accessor value)))

(defmethod initialize-instance ((component direct-value-component) &key data)
  (when data (error "DATA is not allowed for a ~s" (type-of component)))
  (call-next-method)
  (setf (slot-value component 'data) component))

;; TODO: combobox
