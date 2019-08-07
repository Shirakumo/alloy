#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderer.simple)

(defgeneric call-with-pushed-transforms (function renderer))
(defgeneric clip (renderer extent))
(defgeneric translate (renderer point))
(defgeneric scale (renderer size))
(defgeneric rotate (renderer phi))

(defgeneric call-with-pushed-styles (function renderer))
(defgeneric fill-color (renderer))
(defgeneric (setf fill-color) (color renderer))
(defgeneric line-width (renderer))
(defgeneric (setf line-width) (width renderer))
(defgeneric fill-mode (renderer))
(defgeneric (setf fill-mode) (mode renderer))
(defgeneric composite-mode (renderer))
(defgeneric (setf composite-mode) (mode renderer))
(defgeneric font (renderer))
(defgeneric (setf font) (font renderer))
(defgeneric font-size (renderer))
(defgeneric (setf font-size) (size renderer))
;;; TODO: gradients?

(defgeneric line (renderer point-a point-b))
(defgeneric rectangle (renderer extent))
(defgeneric ellipse (renderer extent))
(defgeneric polygon (renderer points))
(defgeneric text (renderer string &key font size))
(defgeneric image (renderer point image &key size))
(defgeneric clear (renderer extent))

(defgeneric wrap-text (renderer string extent &key font size))
(defgeneric request-font (renderer fontspec))
(defgeneric request-image (renderer imagespec))

(defstruct (color
            (:constructor %color (r g b &optional (a 1.0f0)))
            (:conc-name NIL))
  (r 0.0f0 :type single-float)
  (g 0.0f0 :type single-float)
  (b 0.0f0 :type single-float)
  (a 1.0f0 :type single-float))

(defmethod print-object ((color color) stream)
  (format stream "~s" (list 'color (r color) (g color) (b color) (a color))))

(defmethod make-load-form ((color color) &optional env)
  (declare (ignore env))
  (list '%color (r color) (g color) (b color) (a color)))

(defun color (r g b &optional (a 1.0f0))
  (%color (float r 0f0) (float g 0f0) (float b 0f0) (float a 0f0)))

(define-compiler-macro color (r g b &optional (a 0) &environment env)
  (flet ((fold (arg)
           (if (constantp arg env)
               `(load-time-value (float ,arg 0f0))
               `(float ,arg 0f0))))
    `(%color ,(fold r) ,(fold g) ,(fold b) ,(fold a))))

(defclass font ()
  ())

(defclass image ()
  ())

(defclass simple-renderer (alloy:renderer)
  ())

(defmacro with-pushed-transforms ((renderer) &body body)
  `(call-with-pushed-transforms (lambda () ,@body) ,renderer))

(defmacro with-pushed-styles ((renderer) &body body)
  `(call-with-pushed-styles (lambda () ,@body) ,renderer))
