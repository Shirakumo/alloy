#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass default (structure)
  ())

(defmethod initialize-instance :after ((structure default) &key component default)
  (let ((layout (make-instance 'grid :col-sizes '(T 50) :row-sizes '(T)))
        (focus (make-instance 'focus-list))
        (reset (represent "Reset" 'button)))
    (enter component layout)
    (enter reset layout)
    (on activate (reset)
      (setf (value component) default))
    (finish-structure structure layout focus)))
