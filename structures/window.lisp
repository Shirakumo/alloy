#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass window (structure)
  ())

(defclass window-title (label)
  ())

(define-observable close (observable))
(define-observable minimize (observable))
(define-observable maximize (observable))

(defmethod close ((structure window))
  (leave structure T))

(defmethod maximize ((structure window))
  (setf (bounds (layout-element structure)) (bounds (layout-parent (layout-element structure)))))

(defmethod initialize-instance :after ((structure window) &key layout focus focus-parent layout-parent (title "Untitled") (closeable T) (minimizable T) (maximizable T))
  (let ((border-layout (make-instance 'border-layout :layout-parent layout-parent))
        (header (make-instance 'grid-layout :row-sizes '(20) :col-sizes '(T  20 20 20) :cell-margins (margins 2)))
        (focus-stack (make-instance 'focus-stack :focus-parent focus-parent)))
    (enter header border-layout :place :north)
    (enter layout border-layout :place :center)
    (when title
      (enter (represent-with 'window-title title) header :row 0 :col 0))
    (when closeable
      (let ((button (represent "X" 'button)))
        (enter button header :row 0 :col 3)
        (enter button focus-stack :layer 0)
        (on activate (button) (close structure))))
    (when minimizable
      (let ((button (represent "_" 'button)))
        (enter button header :row 0 :col 2)
        (enter button focus-stack :layer 0)
        (on activate (button) (minimize structure))))
    (when maximizable
      (let ((button (represent "o" 'button)))
        (enter button header :row 0 :col 1)
        (enter button focus-stack :layer 0)
        (on activate (button) (maximize structure))))
    (when focus
      (enter focus focus-stack :layer 1))
    (finish-structure structure border-layout focus-stack)))
