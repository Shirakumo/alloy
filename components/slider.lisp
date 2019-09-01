#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass slider (interactable-component)
  ((min :initarg :min :initform 0 :accessor min)
   (max :initarg :max :initform 100 :accessor max)
   (value :initarg :value :initform 0 :accessor value)
   (step :initarg :step :initform 1 :accessor step)
   (state :initform NIL :accessor state)))

(defmethod (setf min) :after (value (slider slider))
  (setf (value slider) (value slider)))

(defmethod (setf max) :after (value (slider slider))
  (setf (value slider) (value slider)))

(defmethod (setf value) :around (value (slider slider))
  (call-next-method (max (min slider) (min (max slider) value)) slider))

(defmethod (setf step) :before (value (slider slider))
  (assert (< 0 value) (value)))

(defmethod handle ((event scroll) (slider slider) ctx)
  (incf (value slider) (* (delta event) (step slider))))

(defmethod handle ((event key-up) (slider slider) ctx)
  (case (key event)
    ((:down :left)
     (decf (value slider) (step slider)))
    ((:up :right)
     (incf (value slider) (step slider)))
    (:home
     (setf (value slider) (min slider)))
    (:end
     (setf (value slider) (max slider)))
    (T
     (call-next-method))))

(defmethod handle ((event pointer-move) (slider slider) ctx)
  (case (state slider)
    (:dragging
     (let ((range (/ (- (point-x (location event))
                        (extent-x (extent-for slider ctx)))
                     (extent-w (extent-for slider ctx)))))
       (setf (value event) (* range (- (max slider) (min slider))))))
    (T
     (call-next-method))))

(defmethod handle ((event pointer-up) (slider slider) ctx)
  (case (state slider)
    (:dragging
     (setf (state slider) NIL))
    (T
     (call-next-method))))

(defmethod handle ((event pointer-down) (slider slider) ctx)
  (setf (state slider) :dragging))

(defmethod exit :after ((slider slider))
  (setf (state slider) NIL))
