#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass draggable (component)
  ((initial-pos :initform NIL :accessor initial-pos)
   (press-offset :initform (px-point) :accessor press-offset)))

(define-observable drag-to (location observable))

(defmethod drag-to (location (draggable draggable))
  )

(defmethod handle ((event pointer-down) (draggable draggable))
  (setf (initial-pos draggable) (px-point (x (bounds draggable)) (y (bounds draggable))))
  (setf (press-offset draggable) (px-point (- (pxx (location event)) (pxx (initial-pos draggable)))
                                           (- (pxy (location event)) (pxy (initial-pos draggable)))))
  (activate draggable))

(defmethod handle ((event pointer-up) (draggable draggable))
  (setf (initial-pos draggable) NIL)
  (exit draggable))

(defmethod handle ((event pointer-move) (draggable draggable))
  (when (initial-pos draggable)
    (let ((nx (- (pxx (location event)) (pxx (press-offset draggable))))
          (ny (- (pxy (location event)) (pxy (press-offset draggable)))))
      (drag-to (px-point nx ny) draggable))))

(defmethod handle ((event button-down) (draggable draggable))
  (case (button event)
    (:a (setf (initial-pos draggable) (px-point (x (bounds draggable)) (y (bounds draggable))))
     (setf (press-offset draggable) (px-point (/ (pxw (bounds draggable)) 2) (/ (pxh (bounds draggable)) 2))))
    (T (call-next-method))))

(defmethod handle ((event button-up) (draggable draggable))
  (case (button event)
    (:a (setf (initial-pos draggable) NIL))
    (T (call-next-method))))
