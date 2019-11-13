#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass tab (element)
  ((name :initarg :name :initform "Untitled" :reader name)
   (layout-element :initarg :layout-element :initform (arg! :layout-element) :reader layout-element)
   (focus-element :initarg :focus-element :initform NIL :reader focus-element)))

(defclass tab-view (structure vector-container)
  ((current-tab :initform (make-instance 'value-data) :reader current-tab)))

(defclass tab-button (radio) ())

(define-observable (setf current) (current observable))

(defmethod current ((structure tab-view))
  (value (current-tab structure)))

(defmethod (setf current) ((tab tab) (structure tab-view))
  (when (focus-element tab)
    (handler-case
        (leave (index-element '(1 . 0) (focus-element structure)) (focus-element structure))
      (index-out-of-range ()))
    (enter (focus-element tab) (focus-element structure) :layer 1))
  (setf (value (current-tab structure)) tab)
  (setf (index (focus-element structure)) '(1 . 0))
  (setf (current (index-element :center (layout-element structure))) (layout-element tab)))

(defmethod enter :after ((tab tab) (structure tab-view) &key)
  (let ((button (represent-with 'tab-button (current-tab structure) :active-value tab)))
    (enter button (index-element :north (layout-element structure)))
    (enter button (focus-element structure) :layer 0)
    (enter (layout-element tab) (index-element :center (layout-element structure)))
    (on activate (button)
      (setf (current structure) tab))))

(defmethod initialize-instance :after ((structure tab-view) &key tabs focus-parent layout-parent)
  (let ((layout (make-instance 'border-layout :layout-parent layout-parent))
        (focus (make-instance 'focus-stack :focus-parent focus-parent))
        (tablist (make-instance 'horizontal-linear-layout :min-size (size 40 20)))
        (stack (make-instance 'swap-layout)))
    (enter tablist layout :place :north)
    (enter stack layout :place :center)
    (finish-structure structure layout focus)
    (when tabs
      (dolist (tab tabs)
        (enter tab structure))
      (setf (current structure) (first tabs)))))
