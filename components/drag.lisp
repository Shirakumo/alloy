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

(defclass resizer (draggable)
  ((side :initarg :side :accessor side)
   (initial-bounds :initform NIL :accessor initial-bounds)))

(defmethod (setf initial-pos) :after (value (resizer resizer))
  (setf (initial-bounds resizer) (if value
                                     (bounds (data resizer))
                                     NIL)))

(defmethod drag-to (location (resizer resizer))
  (let* ((dx (- (pxx location) (pxx (initial-pos resizer))))
         (dy (- (pxy location) (pxy (initial-pos resizer))))
         (target (data resizer))
         (ib (initial-bounds resizer)))
    (ecase (side resizer)
      (:north
       (setf (bounds target) (px-extent (x ib) (y ib) (w ib) (+ (pxh ib) dy))))
      (:east
       (setf (bounds target) (px-extent (x ib) (y ib) (+ (pxw ib) dx) (h ib))))
      (:south
       (setf (bounds target) (px-extent (x ib) (+ (pxy ib) dy) (w ib) (- (pxh ib) dy))))
      (:west
       (setf (bounds target) (px-extent (+ (pxx ib) dx) (y ib) (- (pxw ib) dx) (h ib)))))
    ;; FIXME: We ought to do this, but it causes unwanted springyness in dragged layouts...
    #++(notice-bounds target (layout-parent target))))

(defmethod suggest-bounds (extent (resizer resizer))
  (px-extent (pxx extent) (pxy extent) (un 5) (un 5)))
