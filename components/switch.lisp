#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass bool () ())

(defclass switch (value-component)
  ((off-value :initarg :off :initform NIL :accessor off-value)
   (on-value :initarg :on :initform T :accessor on-value)))

(defmethod activate ((switch switch))
  (setf (value switch) (if (active-p switch)
                           (off-value switch)
                           (on-value switch))))

(defmethod active-p ((switch switch))
  (eql (on-value switch) (value switch)))

(defmethod component-class-for-object ((_ (eql NIL))) 'switch)
(defmethod component-class-for-object ((_ (eql T))) 'switch)
(defmethod component-class-for-object ((_ bool)) 'switch)

(defclass checkbox (switch)
  ())

(defclass labelled-switch (switch)
  ((text :initarg :text :initform "" :accessor text)))
