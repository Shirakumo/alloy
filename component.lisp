#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass component (observable layout-element focus-element)
  ((focus-parent :initform NIL)
   (data :initarg :data :initform (arg! :data) :reader data)))

(defmethod initialize-instance ((component component) &key)
  (call-next-method)
  ;; No focus parent, we don't expect to be focused ever, so
  ;; stub ourselves out with this.
  (unless (focus-parent component)
    (setf (slot-value element 'focus-parent) component)))

(defmethod suggest-bounds (extent (component component))
  extent)

(defmethod handle ((event pointer-down) (component component) ctx)
  (activate component))

(defmethod handle ((event pointer-move) (component component) ctx)
  (unless (eql :strong (focus component))
    (setf (focus component) :weak)))

(defmethod (setf focus) :after (value (component component))
  (mark-for-render component))

(make-observable '(setf focus) '(focus observable))
(make-observable '(setf bounds) '(bounds observable))
(make-observable 'handle '(event observable ctx))
(make-observable 'activate '(observable))
(make-observable 'exit '(observable))

(defgeneric refresh (component))
(defgeneric component-class-for-object (data))
(defgeneric represent-with (component-type data layout focus-chain &rest initargs))

(defmacro represent (place type layout focus-chain &rest initargs)
  `(represent-with ,type
                   ,(expand-place-data place)
                   ,layout ,focus-chain ,@initargs))

(defmethod represent-with ((type (eql T)) (data data) layout focus-chain &rest initargs)
  (let ((class (component-class-for-object (value data))))
    (apply #'represent-with class data layout focus-chain initargs)))

(defmethod represent-with ((class class) data layout focus-chain &rest initargs)
  (apply #'make-instance class
         :data data
         :layout-parent layout
         :focus-parent focus-chain
         initargs))

(defmethod represent-with ((name symbol) data layout focus-chain &rest initargs)
  (apply #'represent-with (find-class name) data layout focus-chain initargs))
