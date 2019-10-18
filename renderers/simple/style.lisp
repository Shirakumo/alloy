#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(defclass style ()
  ((fill :reader fill)
   (line-width :reader line-width)
   (composite-mode :reader composite-mode)))

(defmethod shared-initialize :after ((style style) slots &key fill line-width composite-mode)
  (macrolet ((update-slot (slot)
               `(when ,slot (setf (,slot style) ,slot))))
    (update-slot fill)
    (update-slot line-width)
    (update-slot composite-mode)))

(defmethod initialize-instance :after ((style style) &key parent)
  (macrolet ((init-slot (slot)
               `(unless (slot-boundp style ',slot)
                  (unless parent (error "The style property ~s is not set!"
                                        ',slot))
                  (setf (,slot style) (,slot parent)))))
    (init-slot fill)
    (init-slot line-width)
    (init-slot composite-mode)))

(defmethod (setf fill) ((color colored:color) (style style))
  (setf (slot-value style 'fill) color))

(defmethod (setf fill) ((gradient gradient) (style style))
  (setf (slot-value style 'fill) gradient))

(defmethod (setf line-width) ((size alloy:unit) (style style))
  (setf (slot-value style 'line-width) size))

(defmethod (setf composite-mode) ((mode symbol) (style style))
  (setf (slot-value style 'composite-mode) mode))

(defclass styled-renderer (renderer)
  ((style :accessor style)))

(defmethod initialize-instance :after ((renderer styled-renderer) &key)
  (setf (style renderer) (make-default-style renderer)))

(defgeneric make-default-style (renderer))

(defmethod make-default-style ((renderer styled-renderer))
  (make-instance 'style :fill colors:black
                        :line-width (alloy:un 1)
                        :composite-mode :source-over))

(defmethod call-with-pushed-styles (function (renderer styled-renderer))
  (let ((current (style renderer)))
    (setf (style renderer) (make-instance (class-of current) :parent current))
    (unwind-protect
         (funcall function)
      (setf (style renderer) current))))

(defmethod fill ((renderer styled-renderer))
  (fill (style renderer)))

(defmethod (setf fill) (color (renderer styled-renderer))
  (setf (fill (style renderer)) color))

(defmethod line-width ((renderer styled-renderer))
  (line-width (style renderer)))

(defmethod (setf line-width) (width (renderer styled-renderer))
  (setf (line-width (style renderer)) width))

(defmethod composite-mode ((renderer styled-renderer))
  (composite-mode (style renderer)))

(defmethod (setf composite-mode) (mode (renderer styled-renderer))
  (setf (composite-mode (style renderer)) mode))
