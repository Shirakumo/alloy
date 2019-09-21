#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass component (observable layout-element focus-element renderable)
  ((focus-parent :initform NIL)
   (layout-parent :initform NIL)
   (data :initarg :data :initform (arg! :data) :reader data)))

(defmethod suggest-bounds (extent (component component))
  extent)

(defmethod handle ((event pointer-down) (component component) ctx)
  (when (slot-boundp component 'focus-parent)
    (activate component)))

(defmethod handle ((event pointer-move) (component component) ctx)
  (when (and (slot-boundp component 'focus-parent)
             (eql NIL (focus component)))
    (setf (focus component) :weak)))

(defmethod (setf focus) :after (value (component component))
  (mark-for-render component))

(make-observable '(setf focus) '(focus observable))
(make-observable '(setf bounds) '(bounds observable))
(make-observable 'handle '(event observable ctx))
(make-observable 'activate '(observable))
(make-observable 'exit '(observable))

(defgeneric component-class-for-object (data))
(defgeneric represent-with (component-type data &rest initargs))

(defmacro represent (place type &rest initargs)
  `(represent-with ,type
                   ,(expand-place-data place)
                   ,@initargs))

(defmethod represent-with ((type (eql T)) (data data) &rest initargs)
  (let ((class (component-class-for-object (value data))))
    (apply #'represent-with class data initargs)))

(defmethod represent-with ((class class) data &rest initargs)
  (apply #'make-instance class :data data initargs))

(defmethod represent-with ((name symbol) data &rest initargs)
  (apply #'represent-with (find-class name) data initargs))
