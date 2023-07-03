(in-package #:org.shirakumo.alloy)

(defclass query (structure)
  ())

(defmethod accept ((query query)))

(defmethod initialize-instance :after ((structure query) &key (submit-text "Go") input-data)
  (let ((layout (make-instance 'grid :col-sizes '(T 30) :row-sizes '(T)))
        (focus (make-instance 'focus-list))
        (input (represent input-data 'input-line))
        (submit (represent submit-text 'button)))
    (enter input layout :row 0 :col 0)
    (enter submit layout :row 0 :col 1)
    (on accept (input) (accept structure))
    (on activate (submit) (accept structure))
    (finish-structure structure layout focus)))
