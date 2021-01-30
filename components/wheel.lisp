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
   (grid :initarg :grid :initform 0 :accessor grid)
   (start-pos :initform NIL :accessor start-pos)
   (start-val :initform NIL :accessor start-val)))

(defmethod component-class-for-object ((real real)) (find-class 'wheel))

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
  (cond ((string= "" text) 0)
        ((string= "-" text) -0.0)
        ((string= "+" text) +0.0)
        ((string= "." text) 0.0)
        (T (read-from-string text))))

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

(defmethod handle ((event pointer-down) (wheel wheel))
  (cond ((contained-p (location event) (bounds wheel))
         (activate wheel)
         (call-next-method)
         (setf (start-pos wheel) (location event))
         (setf (start-val wheel) (value wheel)))
        (T
         (decline))))

(defmethod handle ((event pointer-move) (wheel wheel))
  (if (start-pos wheel)
      (with-unit-parent wheel
        (let* ((relative-x (- (pxx (location event)) (pxx (start-pos wheel))))
               (relative-y (- (pxy (location event)) (pxy (start-pos wheel))))
               (diff (/ (sqrt (+ (expt relative-x 2) (expt relative-y 2)))
                        ;; Normalise
                        (to-px (un 20)))))
          ;; dead zone
          (if (< 1 diff)
              (setf (value wheel)
                    (+ (start-val wheel)
                       (* (- diff 1)
                          ;; Diagonal along 0,0 -> 1,-1 to determine whether to increase or decrease.
                          (if (< (- relative-x) relative-y) +1 -1))))
              (setf (value wheel) (start-val wheel)))))
      (call-next-method)))

(defmethod handle ((event pointer-up) (wheel wheel))
  (cond ((start-pos wheel)
         (handle (make-instance 'pointer-move :location (location event) :old-location (location event)) wheel)
         (setf (start-pos wheel) NIL)
         (setf (start-val wheel) NIL))
        (T
         (call-next-method))))

(defclass ranged-wheel (wheel)
  ((range :initarg :range :initform '(0 . 100) :accessor range)))

(defmethod (setf range) :before (value (wheel ranged-wheel))
  (assert (< (car value) (cdr value))))

(defmethod (setf value) (value (wheel ranged-wheel))
  (destructuring-bind (min . max) (range wheel)
    (cond ((and min (< value min)) (call-next-method min wheel))
          ((and max (< max value)) (call-next-method max wheel))
          (T (call-next-method value wheel)))))
