#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

(defclass font ()
  ((family :initarg :family :initform (arg! :family) :reader family)
   (slant :initarg :slant :initform :roman :reader slant)
   (spacing :initarg :spacing :initform :proportional :reader spacing)
   (weight :initarg :weight :initform :regular :reader weight)
   (stretch :initarg :stretch :initform :normal :reader stretch)))

(defmethod request-font ((renderer renderer) (family string) &key slant spacing weight stretch)
  (make-instance 'font :family family :slant slant :spacing spacing :weight weight :stretch stretch))

(defclass image ()
  ((size :initarg :size :reader size)
   (data :initarg :data :reader data)))

(defmethod request-image ((renderer renderer) (data vector) &key size)
  (make-instance 'image :data data :size size))

(defclass gradient ()
  ((start :initarg :start :initform (arg! :start) :reader start)
   (stop :initarg :stop :initform (arg! :stop) :reader stop)
   (stops :initarg :stops :initform (arg! :stops) :reader stops)))

(defclass linear-gradient (gradient) ())
(defclass radial-gradient (gradient) ())
(defclass angle-gradient (gradient) ())
(defclass diamond-gradient (gradient) ())

(defmethod request-gradient ((renderer renderer) type start stop stops &key)
  (make-instance type :start start :stop stop :stops stops))

(defclass shape () ())
(defclass patterned-shape (shape)
  ((pattern :initarg :pattern :initform colors:black :accessor pattern)))
(defclass filled-shape (patterned-shape) ())
(defclass outlined-shape (patterned-shape)
  ((line-width :initarg :line-width :initform (alloy:un 1) :accessor line-width)))

(defclass rectangle (shape)
  ((bounds :initarg :bounds :initform (arg! :bounds) :accessor bounds)))

(defclass filled-rectangle (rectangle filled-shape) ())
(defclass outlined-rectangle (rectangle outlined-shape) ())

(defmethod rectangle ((renderer renderer) (bounds alloy:extent) &key pattern line-width)
  (if line-width
      (make-instance 'outlined-rectangle :bounds bounds :pattern pattern :line-width line-width)
      (make-instance 'filled-rectangle :bounds bounds :pattern pattern)))

(defclass ellipse (shape)
  ((bounds :initarg :bounds :initform (arg! :bounds) :accessor bounds)))

(defclass filled-ellipse (ellipse filled-shape) ())
(defclass outlined-ellipse (ellipse outlined-shape) ())

(defmethod ellipse ((renderer renderer) (bounds alloy:extent) &key pattern line-width)
  (if line-width
      (make-instance 'outlined-ellipse :bounds bounds :line-width line-width)
      (make-instance 'filled-ellipse :bounds bounds)))

(defclass polygon (filled-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod polygon ((renderer renderer) (points vector) &key pattern)
  (make-instance 'outlined-polygon :points points :pattern pattern))

(defclass line-strip (outlined-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod line-strip ((renderer renderer) (points vector) &key line-width)
  (make-instance 'line-strip :points points :line-width line-width))

(defclass curve (outlined-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod curve ((renderer renderer) (points vector) &key line-width)
  (make-instance 'curve :points points :line-width line-width))

;; FIXME: changing styles and size, multiple styles and size per text
(defclass text (shape)
  ((text :initarg :text :initform (arg! :text) :accessor text)
   (font :initarg :font :initform (arg! :font) :accessor font)
   (size :initarg :size :initform (arg! :size) :accessor size)
   (bounds :initarg :bounds :initform (alloy:margins) :accessor bounds)
   (valign :initarg :valign :initform :middle :accessor valign)
   (halign :initarg :halign :initform :start :accessor halign)
   (direction :initarg :direction :initform :right :accessor direction)))

(defmethod text ((renderer renderer) (point alloy:point) (string string) &key (font (request-font renderer :default))
                                                                              (size (alloy:un 10))
                                                                              (bounds (alloy:margins))
                                                                              (align '(:middle :left))
                                                                              (direction :right))
  (destructuring-bind (valign halign) align
    (make-instance 'text :text text :font font :size size
                         :bounds bounds :direction direction
                         :valign valign :halign halign)))

(defclass icon (shape)
  ((image :initarg :image :initform (arg! :image) :accessor image)
   (size :initarg :size :initform (arg! :size) :accessor size)
   (bounds :initarg :bounds :initform (alloy:margins) :accessor bounds)
   (valign :initarg :valign :initform :middle :accessor valign)
   (halign :initarg :halign :initform :left :accessor halign)))

(defmethod icon ((renderer renderer) (point alloy:point) (image image) &key (size (size image))
                                                                            (bounds (alloy:margins))
                                                                            (align '(:middle :left)))
  (destructuring-bind (valign halign) align
    (make-instance 'icon :image image :size size :bounds bounds
                         :valign valign :halign halign)))

(defun resolve-alignment (extent halign valign size)
  (alloy:px-point
   (+ (alloy:pxx extent)
      (ecase halign
        (:start 0)
        (:middle (/ (- (alloy:pxw extent) (alloy:pxw size)) 2))
        (:end (- (alloy:pxw extent) (alloy:pxw size)))))
   (+ (alloy:pxy extent)
      (ecase valign
        (:bottom 0)
        (:middle (/ (- (alloy:pxh extent) (alloy:pxh size)) 2))
        (:top (- (alloy:pxh extent) (alloy:pxh size)))))))
