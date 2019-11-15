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

(defmethod bounds ((structure structure)) (bounds (layout-element structure)))
(defmethod (setf bounds) (value (structure structure)) (setf (bounds (layout-element structure)) value))
(defmethod x ((structure structure)) (x (layout-element structure)))
(defmethod y ((structure structure)) (y (layout-element structure)))
(defmethod w ((structure structure)) (w (layout-element structure)))
(defmethod h ((structure structure)) (h (layout-element structure)))

(defmethod register ((structure structure) (renderer renderer))
  (register (layout-element structure) renderer))

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
