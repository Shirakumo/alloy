#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass window (structure)
  ())

(define-observable close (observable))
(define-observable minimize (observable))
(define-observable maximize (observable))

(defmethod close ((structure window))
  (leave structure T))

(defmethod maximize ((structure window))
  (setf (bounds (border-element structure)) (bounds (layout-parent (border-element structure)))))

(defmethod initialize-instance :after ((structure window) &key layout focus (title "Untitled") (closeable T) (minimizable T) (maximizable T))
  (let ((border-layout (make-instance 'border-layout))
        (header (make-instance 'grid-layout :row-sizes '(20) :col-sizes '(T  20 20 20)))
        (focus-grid (make-instance 'focus-grid :width 3)))
    (enter header border-layout :north)
    (enter layout border-layout :center)
    (when title
      (enter (represent title 'label) header :row 0 :col 0))
    (when closeable
      (let ((button (represent "X" 'button)))
        (enter button header :row 0 :col 3)
        (enter button focus-grid)
        (on activate (button) (close structure))))
    (when minimizable
      (let ((button (represent "_" 'button)))
        (enter button header :row 0 :col 2)
        (enter button focus-grid)
        (on activate (button) (minimize structure))))
    (when maximizable
      (let ((button (represent "o" 'button)))
        (enter button header :row 0 :col 1)
        (enter button focus-grid)
        (on activate (button) (maximize structure))))
    (when focus
      (enter focus focus-grid))
    (finish-structure structure border-layout focus-grid)))
