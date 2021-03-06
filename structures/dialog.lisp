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
  (let ((layout (make-instance 'horizontal-linear-layout :align :end :cell-margins (margins 5 0))))
    (when accept
      (let ((accept (represent accept 'button)))
        (on activate (accept)
          (accept dialog))
        (enter accept layout)
        (enter accept (focus-element dialog) :layer 2)))
    (when reject
      (let ((reject (represent reject 'button)))
        (on activate (reject)
          (reject dialog))
        (enter reject layout)
        (enter reject (focus-element dialog) :layer 2)))
    (leave (index-element :south (layout-element dialog)) (layout-element dialog))
    (enter layout (layout-element dialog) :place :south :size (un 20))))

(defclass dialog* (dialog)
  ((on-accept :initarg :on-accept :initform #'identity :accessor on-accept)
   (on-reject :initarg :on-reject :initform #'identity :accessor on-reject)))

(defmethod accept ((dialog dialog*))
  (funcall (on-accept dialog) dialog))

(defmethod reject ((dialog dialog*))
  (funcall (on-reject dialog) dialog))

(defun confirm (text on-ok &rest initargs &key (title "Confirm") &allow-other-keys)
  (apply #'make-instance 'dialog* :title title :on-accept on-ok
                                  :layout (make-instance 'label* :wrap T :value text)
                                  initargs))

(defun message (text &rest initargs &key (title "Message") &allow-other-keys)
  (apply #'make-instance 'dialog* :title title :reject NIL :layout (make-instance 'label* :wrap T :value text) initargs))

(defmacro with-confirmation ((text &rest initargs) &body body)
  (let ((dialog (gensym "DIALOG")))
    `(confirm ,text
              (lambda (,dialog)
                (declare (ignore ,dialog))
                ,@body)
              ,@initargs)))
