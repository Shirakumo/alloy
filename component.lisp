#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass component (observable layout-element focus-element renderable)
  ((data :initarg :data :initform (arg! :data) :accessor data)
   (ideal-size :initarg :ideal-size :initform NIL :accessor ideal-size)))

(defmethod print-object ((element component) stream)
  (print-unreadable-object (element stream :type T :identity T)
    (format stream "~a ~a" (bounds element) (focus element))))

(defmethod suggest-size ((size size) (component component))
  (or (preferred-size component)
      size))

(defmethod preferred-size ((component component))
  (ideal-size component))

(defmethod handle ((event pointer-down) (component component))
  (unless (and (slot-boundp component 'focus-parent)
               (contained-p (location event) component))
    (call-next-method)))

(defmethod handle ((event pointer-up) (component component))
  (if (and (slot-boundp component 'focus-parent)
           (contained-p (location event) component)
           (eql :left (kind event)))
      (activate component)
      (call-next-method)))

(defmethod handle ((event pointer-move) (component component))
  (if (and (slot-boundp component 'focus-parent)
           (contained-p (location event) component))
      (when (eql NIL (focus component))
        (setf (focus (focus-parent component)) :strong)
        (setf (focus component) :weak))
      (call-next-method)))

(defmethod maybe-render ((renderer renderer) (component component)))

(defmethod (setf data) :after (value (component component))
  (mark-for-render component))

(defmethod (setf focus) :after (value (component component))
  (when (and value (layout-tree component))
    (ensure-visible component T))
  (mark-for-render component))

(defmethod (setf bounds) :after (value (component component))
  (mark-for-render component))

(make-observable '(setf focus) '(focus observable))
(make-observable '(setf bounds) '(bounds observable))
;(make-observable 'handle '(event observable))
(make-observable 'activate '(observable))
(make-observable 'exit '(observable))

(defgeneric component-class-for-object (data))
(defgeneric represent-with (component-type data &rest initargs))
(defgeneric represent-for (component data &rest initargs))

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

(defmethod leave ((component component) (parent (eql T)))
  (when (slot-boundp component 'layout-parent)
    (leave component (layout-parent component)))
  (when (slot-boundp component 'focus-parent)
    (leave component (focus-parent component))))
