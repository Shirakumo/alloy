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
        (destructuring-bind (name &optional type &rest initargs) (if (listp slot) slot (list slot))
          (when (slot-boundp object name)
            (etypecase type
              (class)
              (null
               (let ((slot-type (c2mop:slot-definition-type (find-slot name class))))
                 (when (and (symbolp slot-type) (not (eql T slot-type)) (not (eql NIL slot-type)))
                   ;; FIXME: We can do better here if we analyse compound types
                   (setf type (find-class slot-type)))))
              (symbol
               (setf type (find-class type))))
            (when type
              (let ((component (apply #'represent-with
                                      (component-class-for-object (c2mop:class-prototype type))
                                      (make-instance 'slot-data :object object :slot name)
                                      initargs)))
                (enter (string name) layout)
                (enter component layout)
                (enter component focus)))))))))

(defmethod initialize-instance :after ((structure inspector) &key focus-parent layout-parent)
  (let ((layout (make-instance 'grid-layout :col-sizes '(100 T) :row-sizes '(30) :layout-parent layout-parent))
        (focus (make-instance 'focus-list :focus-parent focus-parent)))
    (finish-structure structure layout focus)
    (update-inspector-slots structure)))

(defmethod reinitialize-instance :after ((structure inspector) &key)
  (update-inspector-slots structure))
