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

(defclass wheel (input-line)
  ((step :initarg :step :initform 1 :accessor step)
   (grid :initarg :grid :initform 0 :accessor grid)
   (text :initarg :text :initform "" :accessor text)))

(defmethod initialize-instance :after ((wheel wheel) &key)
  (setf (text wheel) (prin1-to-string (value wheel))))

(defmethod (setf value) :around (value (wheel wheel))
  (let ((value (if (< 0 (grid wheel))
                   (* (round (/ value (grid wheel))) (grid wheel))
                   value)))
    (call-next-method (make-number-like (value wheel) value) wheel)))

(defmethod accept :before ((wheel wheel))
  (let ((text (string-trim " " (text wheel))))
    (cond ((string= "" text)
           (setf (value wheel) 0))
          ((and (every (lambda (c) (find c "0123456789.")) text)
                (<= (count #\. text) 1))
           (setf (value wheel) (read-from-string text)))
          (T
           (warn "Invalidly formatted wheel input: ~a" text)
           ;; KLUDGE: Invalid format: what do we do here?
           ))))

(defmethod mark-for-render :after ((wheel wheel))
  ;; (unless (eql :strong (focus wheel))
  ;;   (setf (text wheel) (prin1-to-string (value wheel))))
  )

(defmethod (setf focus) :after (focus (wheel wheel))
  (unless (eql :strong (focus wheel))
    (setf (text wheel) (prin1-to-string (value wheel)))))

(defmethod (setf text) ((text string) (wheel wheel))
  (let ((found-dot (typep (value wheel) 'integer)))
    (flet ((character-ok-p (c)
             (if (char= #\. c)
                 (unless found-dot
                   (setf found-dot T))
                 (find c "0123456789"))))
      (call-next-method (remove-if-not #'character-ok-p text) wheel))))

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
