#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass query (structure)
  ())

(defmethod accept ((query query)))

(defmethod initialize-instance :after ((structure query) &key (submit-text "Go") input-data)
  (let ((layout (make-instance 'grid :col-sizes '(T 30) :row-sizes '(T)))
        (focus (make-instance 'focus-list))
        (input (represent input-data 'input-line))
        (submit (represent "Go" 'button)))
    (enter input layout :row 0 :col 0)
    (enter submit layout :row 0 :col 1)
    (on accept (input) (accept structure))
    (on activate (submit) (accept structure))
    (finish-structure structure layout focus)))
