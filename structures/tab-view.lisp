#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass tab-view (structure)
  ())

(defclass tab-button (radio) ())

(define-observable (setf index) (index observable))

(defmethod index ((structure tab-view))
  (index (focus-element structure)))

(defmethod (setf index) (value (structure tab-view))
  ;; FIXME: does not change current tab
  (setf (index (focus-element structure)) value)
  (setf (index (index-element :center (layout-element structure))) value))

(defmethod initialize-instance :after ((structure tab-view) &key tabs)
  (let ((layout (make-instance 'border-layout))
        (tablist (make-instance 'horizontal-list-layout))
        (stack (make-instance 'stack-layout))
        (focus-list (make-instance 'focus-list))
        (current-tab (make-instance 'value-data :value (caar tabs))))
    (enter tablist layout :north)
    (enter stack layout :center)
    (loop for i from 0
          for (name layout focus) in tabs
          for button = (represent current-tab 'tab-button :active-value name)
          do (enter layout stack)
             (when focus
               (enter focus focus-list))
             (enter button tablist)
             (on activate (button)
               (setf (index structure) i)))
    (finish-structure structure layout focus-list)))
