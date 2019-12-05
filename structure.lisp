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

(macrolet ((define-deferral (gf target wrapper &optional rest-p)
             `(defmethod ,gf ((structure structure) (,target ,target) ,@(when rest-p '(&rest initargs)))
                ,(if rest-p
                     `(apply #',gf (,wrapper structure) ,target initargs)
                     `(,gf (,wrapper structure) ,target)))))
  (define-deferral enter layout-element layout-element &rest)
  (define-deferral enter layout-tree layout-element &rest)
  (define-deferral enter focus-element focus-element &rest)
  (define-deferral enter focus-tree focus-element &rest)
  (define-deferral update layout-element layout-element &rest)
  (define-deferral update layout-tree layout-element &rest)
  (define-deferral update focus-element focus-element &rest)
  (define-deferral update focus-tree focus-element &rest)
  (define-deferral leave layout-element layout-element)
  (define-deferral leave layout-tree layout-element)
  (define-deferral leave focus-element focus-element)
  (define-deferral leave focus-tree focus-element))

(defmethod leave ((structure structure) (self (eql T)))
  (leave (layout-element structure) (layout-parent (layout-element structure)))
  (leave (focus-element structure) (focus-parent (focus-element structure))))
