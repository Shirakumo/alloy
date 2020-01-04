#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defun make-number-like (target a)
  (etypecase target
    (long-float (float a 0l0))
    (double-float (float a 0d0))
    (single-float (float a 0f0))
    (short-float (float a 0s0))
    (integer (round a))
    (ratio (rational a))))

(defclass wheel (input-line filtered-text-input validated-text-input transformed-text-input)
  ((step :initarg :step :initform 1 :accessor step)
   (grid :initarg :grid :initform 0 :accessor grid)))

(defmethod (setf value) :around (value (wheel wheel))
  (let ((value (if (< 0 (grid wheel))
                   (* (round (/ value (grid wheel))) (grid wheel))
                   value)))
    (call-next-method (make-number-like (value wheel) value) wheel)))

(defmethod accept-character ((wheel wheel) c &optional state)
  (destructuring-bind (&optional found-dot) state
    (values
     (case c
       (#\.
        (unless found-dot
          (setf found-dot T)))
       (#\-
        (null state))
       (T
        (find c "0123456789")))
     (list found-dot))))

(defmethod value->text ((wheel wheel) value)
  (etypecase value
    (integer (prin1-to-string value))
    (float (format NIL "~f" value))))

(defmethod text->value ((wheel wheel) text)
  (if (string= "" text)
      0
      (read-from-string text)))

(defmethod (setf step) :before (value (wheel wheel))
  (assert (< 0 value) (value)))

(defmethod (setf grid) :before (value (wheel wheel))
  (assert (<= 0 value) (value)))

(defmethod handle ((event scroll) (wheel wheel))
  (incf (value wheel) (* (dy event) (step wheel))))

(defmethod handle ((event key-up) (wheel wheel))
  (case (key event)
    ((:down)
     (decf (value wheel) (step wheel)))
    ((:up)
     (incf (value wheel) (step wheel)))
    (T
     (call-next-method))))
