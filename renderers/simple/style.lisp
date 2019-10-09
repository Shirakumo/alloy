#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(defclass style ()
  ((fill-color :reader fill-color)
   (font :reader font)
   (font-size :reader font-size)
   (line-width :reader line-width)
   (fill-mode :reader fill-mode)
   (composite-mode :reader composite-mode)))

(defmethod shared-initialize :after ((style style) slots &key fill-color font font-size line-width fill-mode composite-mode)
  (macrolet ((update-slot (slot)
               `(when ,slot (setf (,slot style) ,slot))))
    (update-slot fill-color)
    (update-slot font)
    (update-slot font-size)
    (update-slot line-width)
    (update-slot fill-mode)
    (update-slot composite-mode)))

(defmethod initialize-instance :after ((style style) &key parent)
  (macrolet ((init-slot (slot)
               `(unless (slot-boundp style ',slot)
                  (unless parent (error "The style property ~s is not set!"
                                        ',slot))
                  (setf (,slot style) (,slot parent)))))
    (init-slot fill-color)
    (init-slot font)
    (init-slot font-size)
    (init-slot line-width)
    (init-slot fill-mode)
    (init-slot composite-mode)))

(defmethod (setf fill-color) ((color color) (style style))
  (setf (slot-value style 'fill-color) color))

(defmethod (setf line-width) ((width float) (style style))
  (setf (slot-value style 'line-width) width))

(defmethod (setf line-width) ((size alloy:unit) (style style))
  (setf (slot-value style 'line-width) size))

(defmethod (setf fill-mode) ((mode symbol) (style style))
  (setf (slot-value style 'fill-mode) mode))

(defmethod (setf composite-mode) ((mode symbol) (style style))
  (setf (slot-value style 'composite-mode) mode))

(defmethod (setf font) ((font font) (style style))
  (setf (slot-value style 'font) font))

(defmethod (setf font-size) ((size float) (style style))
  (setf (slot-value style 'font-size) size))

(defmethod (setf font-size) ((size alloy:unit) (style style))
  (setf (slot-value style 'font-size) size))

(defclass styled-renderer (renderer)
  ((style :accessor style)))

(defmethod initialize-instance :after ((renderer styled-renderer) &key)
  (setf (style renderer) (make-default-style renderer)))

(defgeneric make-default-style (renderer))

(defmethod make-default-style ((renderer styled-renderer))
  (make-instance 'style :fill-color (color 0 0 0)
                        :line-width (alloy:un 1)
                        :fill-mode :lines
                        :composite-mode :source-over
                        :font (request-font renderer :default)
                        :font-size (alloy:un 12)))

(defmethod call-with-pushed-styles (function (renderer styled-renderer))
  (let ((current (style renderer)))
    (setf (style renderer) (make-instance (class-of current) :parent current))
    (unwind-protect
         (funcall function)
      (setf (style renderer) current))))

(defmethod fill-color ((renderer styled-renderer))
  (fill-color (style renderer)))

(defmethod (setf fill-color) (color (renderer styled-renderer))
  (setf (fill-color (style renderer)) color))

(defmethod line-width ((renderer styled-renderer))
  (line-width (style renderer)))

(defmethod (setf line-width) (width (renderer styled-renderer))
  (setf (line-width (style renderer)) width))

(defmethod fill-mode ((renderer styled-renderer))
  (fill-mode (style renderer)))

(defmethod (setf fill-mode) (mode (renderer styled-renderer))
  (setf (fill-mode (style renderer)) mode))

(defmethod composite-mode ((renderer styled-renderer))
  (composite-mode (style renderer)))

(defmethod (setf composite-mode) (mode (renderer styled-renderer))
  (setf (composite-mode (style renderer)) mode))

(defmethod font ((renderer styled-renderer))
  (font (style renderer)))

(defmethod (setf font) (font (renderer styled-renderer))
  (setf (font (style renderer)) font))

(defmethod font-size ((renderer styled-renderer))
  (font-size (style renderer)))

(defmethod (setf font-size) (size (renderer styled-renderer))
  (setf (font-size (style renderer)) size))
