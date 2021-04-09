#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass inspector (structure)
  ((object :initarg :object :initform NIL)
   (class :initarg :class)
   (slots :initarg :slots :initform :all)))

(defun find-slot (name class)
  (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name))

(defgeneric find-canonical-class (type))

(defmethod find-canonical-class ((type symbol))
  (find-class type))

(defmethod find-canonical-class ((type (eql 'boolean)))
  (find-class 'bool))

(defgeneric object-slot-component (object class slot))

(defmethod object-slot-component-type (object class slot)
  (let ((slot-type (c2mop:slot-definition-type (find-slot slot class))))
    (when (and (symbolp slot-type) (not (eql T slot-type)) (not (eql NIL slot-type)))
      ;; FIXME: We can do better here if we analyse compound types
      (component-class-for-object (c2mop:class-prototype (find-canonical-class slot-type))))))

(defmethod object-slot-component (object class slot)
  (when (slot-boundp object slot)
    (let ((type (object-slot-component-type object class slot)))
      (when type
        (represent-with
         type
         (if (and (fboundp slot) (fboundp `(setf ,slot)))
             (make-instance 'accessor-data :object object :accessor slot)
             (make-instance 'slot-data :object object :slot slot)))))))

(defun update-inspector-slots (inspector)
  (let* ((layout (layout-element inspector))
         (focus (focus-element inspector))
         (object (slot-value inspector 'object))
         (class (if (slot-boundp inspector 'class)
                    (slot-value inspector 'class)
                    (class-of object)))
         (slots (slot-value inspector 'slots)))
    (case slots
      (:all
       (setf slots (mapcar #'c2mop:slot-definition-name (c2mop:class-slots class))))
      (:direct
       (setf slots (mapcar #'c2mop:slot-definition-name (c2mop:class-direct-slots class)))))
    (dolist (slot slots)
      (with-simple-restart (continue "Ignore the slot ~a" slot)
        (let ((component (object-slot-component object class slot)))
          (when component
            (enter (string slot) layout)
            (enter component layout)
            (enter component focus)))))))

(defmethod initialize-instance :after ((structure inspector) &key focus-parent layout-parent)
  (let ((layout (make-instance 'grid-layout :col-sizes '(100 T) :row-sizes '(30) :layout-parent layout-parent))
        (focus (make-instance 'focus-list :focus-parent focus-parent)))
    (finish-structure structure layout focus)
    (update-inspector-slots structure)))

(defmethod reinitialize-instance :after ((structure inspector) &key)
  (clear (focus-element structure))
  (clear (layout-element structure))
  (update-inspector-slots structure))
