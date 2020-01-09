#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass dialog (window)
  ()
  (:default-initargs
   :minimizable NIL
   :maximizable NIL))

(defmethod accept :after ((dialog dialog))
  (leave dialog T))

(defmethod reject :after ((dialog dialog))
  (leave dialog T))

(defmethod initialize-instance :after ((dialog dialog) &key (accept "Ok") (reject "Cancel"))
  (let ((layout (make-instance 'horizontal-linear-layout :align :end :cell-margins (margins 5 0)))
        (accept (represent accept 'button))
        (reject (represent reject 'button)))
    (on activate (accept)
      (accept dialog))
    (on activate (reject)
      (reject dialog))
    (when reject
      (enter reject layout))
    (when accept
      (enter accept layout))
    (enter layout (layout-element dialog) :place :south)
    (enter accept (focus-element dialog) :layer 2)
    (enter reject (focus-element dialog) :layer 2)))

(defclass dialog* (dialog)
  ((on-accept :initarg :on-accept :initform #'identity :accessor on-accept)
   (on-reject :initarg :on-reject :initform #'identity :accessor on-reject)))

(defmethod accept ((dialog dialog*))
  (funcall (on-accept dialog) dialog))

(defmethod reject ((dialog dialog*))
  (funcall (on-reject dialog) dialog))
