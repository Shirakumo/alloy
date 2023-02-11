#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass button (value-component)
  ((pressed :initform NIL :accessor pressed)))

(defmethod (setf pressed) :after (value (button button))
  (mark-for-render button))

(defmethod handle ((event pointer-move) (button button))
  (if (contained-p (location event) button)
      (setf (cursor (ui button)) :hand)
      (setf (cursor (ui button)) NIL))
  (call-next-method))

(defmethod handle ((event pointer-down) (button button))
  (with-global-bounds (bounds button)
    (if (contained-p (location event) bounds)
        (setf (pressed button) T)
        (call-next-method))))

(defmethod handle ((event pointer-up) (button button))
  (cond ((pressed button)
         (activate button)
         (setf (pressed button) NIL))
        (T
         (exit button))))

(defmethod handle ((event button-down) (button button))
  (case (button event)
    (:a (setf (pressed button) T))
    (T (call-next-method))))

(defmethod handle ((event button-up) (button button))
  (case (button event)
    (:a (setf (pressed button) NIL))
    (T (call-next-method))))

(defmethod (setf focus) :after (value (button button))
  (if (eql value :strong)
      (exit button)
      (setf (pressed button) NIL)))

(defmethod activate ((button button))
  button)

(defclass button* (direct-value-component button)
  ((on-activate :initarg :on-activate :initform (arg! :on-activate) :accessor on-activate)))

(defmethod activate ((button button*))
  (funcall (on-activate button)))
