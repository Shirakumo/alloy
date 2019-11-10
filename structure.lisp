#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass structure (observable)
  ((layout-element :reader layout-element)
   (focus-element :reader focus-element)))

(defun finish-structure (structure layout focus)
  (setf (slot-value structure 'layout-element) layout)
  (setf (slot-value structure 'focus-element) focus))

(defmethod enter ((structure structure) (element layout-element) &rest initargs)
  (apply #'enter (layout-element structure) element initargs))

(defmethod enter ((structure structure) (element focus-element) &rest initargs)
  (apply #'enter (focus-element structure) element initargs))

(defmethod update ((structure structure) (element layout-element) &rest initargs)
  (apply #'update (layout-element structure) element initargs))

(defmethod update ((structure structure) (element focus-element) &rest initargs)
  (apply #'update (focus-element structure) element initargs))

(defmethod leave ((structure structure) (element layout-element))
  (leave (layout-element structure) element))

(defmethod leave ((structure structure) (element focus-element))
  (leave (focus-element structure) element))

(defmethod leave ((structure structure) (self (eql T)))
  (leave (layout-element structure) (layout-parent (layout-element structure)))
  (leave (focus-element structure) (focus-parent (focus-element structure))))
