#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass switch (value-component) ())

(defmethod activate ((switch switch))
  (setf (value switch) (not (value switch))))

(defmethod component-class-for-object ((_ (eql NIL))) 'switch)
(defmethod component-class-for-object ((_ (eql T))) 'switch)
