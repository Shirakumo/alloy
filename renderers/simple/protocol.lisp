#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(defgeneric call-with-pushed-transforms (function renderer))
(defgeneric clip (renderer extent))
(defgeneric translate (renderer point))
(defgeneric scale (renderer size))
(defgeneric rotate (renderer phi))
(defgeneric z-index (renderer))
(defgeneric (setf z-index) (z-index renderer))

(defgeneric call-with-pushed-styles (function renderer))
(defgeneric fill (renderer))
(defgeneric (setf fill) (fill renderer))
(defgeneric line-width (renderer))
(defgeneric (setf line-width) (width renderer))
(defgeneric composite-mode (renderer))
(defgeneric (setf composite-mode) (mode renderer))

(defgeneric clear (renderer extent &allow-other-keys))
(defgeneric line-strip (renderer points &allow-other-keys))
(defgeneric curve (renderer points &allow-other-keys))
(defgeneric rectangle (renderer extent &key filled &allow-other-keys))
(defgeneric ellipse (renderer extent &key filled &allow-other-keys))
(defgeneric polygon (renderer points &key filled &allow-other-keys))
(defgeneric icon (renderer point image &key size extent align &allow-other-keys))
(defgeneric text (renderer point string &key font size extent align direction &allow-other-keys))
(defgeneric cursor (text position &allow-other-keys))
(defgeneric selection (text start end &allow-other-keys))

(defgeneric request-font (renderer family &key slant spacing weight stretch &allow-other-keys))
(defgeneric request-image (renderer data &key size &allow-other-keys))
(defgeneric request-gradient (renderer type start stop stops &allow-other-keys))

(defclass renderer (alloy:renderer)
  ())

(defmethod alloy:component-class-for-object ((image image))
  (find-class 'alloy:icon))

(defmacro with-pushed-transforms ((renderer) &body body)
  `(call-with-pushed-transforms (lambda () ,@body) ,renderer))

(defmacro with-pushed-styles ((renderer) &body body)
  `(call-with-pushed-styles (lambda () ,@body) ,renderer))

(defmethod alloy:call-with-constrained-visibility (function (extent alloy:extent) (renderer renderer))
  (with-pushed-transforms (renderer)
    (clip renderer extent)
    (call-next-method)))
