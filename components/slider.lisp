#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass slider (value-component)
  ((step :initarg :step :initform 1 :accessor step)
   (grid :initarg :grid :initform 0 :accessor grid)
   (state :initform NIL :accessor state)
   (orientation :initarg :orientation :initform :horizontal :accessor orientation)))

(defgeneric range (data))
(define-observable (setf range) (range observable))

(defmethod initialize-instance :after ((slider slider) &key)
  (on (setf range) (range (data slider))
    (setf (value slider) (value slider))))

(defmethod minimum ((slider slider))
  (car (range slider)))

(defmethod maximum ((slider slider))
  (cdr (range slider)))

(defmethod slider-unit ((slider slider))
  (destructuring-bind (min . max) (range slider)
    (if (= min max)
        0
        (max 0 (min 1 (/ (- (value slider) min) (- max min)))))))

(defmethod (setf value) :around (value (slider slider))
  (destructuring-bind (min . max) (range slider)
    (let ((value (if (< 0 (grid slider))
		     (+ min (* (round (- value min) (grid slider)) (grid slider)))
		     value)))
      (call-next-method (max min (min max value)) slider))))

(defmethod (setf value) :after (value (slider slider))
  (mark-for-render slider))

(defmethod (setf step) :before (value (slider slider))
  (assert (< 0 value) (value)))

(defmethod (setf grid) :before (value (slider slider))
  (assert (<= 0 value) (value)))

(defmethod handle ((event scroll) (slider slider))
  (incf (value slider) (* (dy event) (step slider))))

(defmethod handle ((event key-up) (slider slider))
  (case (key event)
    ((:down :left)
     (decf (value slider) (step slider)))
    ((:up :right)
     (incf (value slider) (step slider)))
    (:home
     (setf (value slider) (minimum slider)))
    (:end
     (setf (value slider) (maximum slider)))
    (T
     (call-next-method))))

(defmethod handle ((event pointer-move) (slider slider))
  (case (state slider)
    (:dragging
     (let ((range (ecase (orientation slider)
                    (:horizontal (/ (- (pxx (location event)) (pxx (bounds slider)))
                                    (pxw (bounds slider))))
                    (:vertical (/ (- (pxy (location event)) (pxy (bounds slider)))
                                  (pxh (bounds slider)))))))
       (setf (value slider) (+ (minimum slider) (* range (- (maximum slider) (minimum slider)))))))
    (T
     (call-next-method))))

(defmethod handle ((event pointer-up) (slider slider))
  (case (state slider)
    (:dragging
     (handle (make-instance 'pointer-move :location (location event) :old-location (location event)) slider)
     (setf (state slider) NIL))
    (T
     (call-next-method))))

(defmethod handle ((event pointer-down) (slider slider))
  (call-next-method)
  (setf (state slider) :dragging))

(defclass ranged-slider (slider)
  ((range :initarg :range :initform '(0 . 100) :accessor range)))

(defmethod (setf range) :before (value (slider ranged-slider))
  (assert (< (car value) (cdr value))))
