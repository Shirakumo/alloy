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

(define-observable (setf index) (index observable))

(defmethod index ((structure tab-view))
  (index (focus-element structure)))

(defmethod (setf index) (value (structure tab-view))
  ;; FIXME: does not change current tab
  (setf (index (focus-element structure)) value)
  (setf (index (index-element :center (layout-element structure))) value))

(defmethod enter :after ((tab tab) (structure tab-view) &key)
  (let ((button (represent-with 'tab-button (current-tab structure) :active-value (name tab))))
    (enter button (index-element :north (layout-element structure)))
    (enter button (focus-element structure))
    (on activate (button)
      (setf (index structure) (element-index tab structure)))
    (enter (layout-element tab) (index-element :center (layout-element structure)))
    (when (focus-element tab)
      (enter (focus-element tab) (focus-element structure)))))

(defmethod initialize-instance :after ((structure tab-view) &key tabs focus-parent layout-parent)
  (let ((layout (make-instance 'border-layout :layout-parent layout-parent))
        (focus-list (make-instance 'focus-list :focus-parent focus-parent))
        (tablist (make-instance 'horizontal-linear-layout :min-size (size 40 20)))
        (stack (make-instance 'stack-layout)))
    (enter tablist layout :place :north)
    (enter stack layout :place :center)
    (finish-structure structure layout focus-list)
    (when tabs
      (setf (value (current-tab structure)) (name (first tabs)))
      (dolist (tab tabs)
        (enter tab structure)))))
