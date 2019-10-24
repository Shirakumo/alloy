#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(defclass font ()
  ((family :initarg :family :initform (arg! :family) :reader family)
   (slant :initarg :slant :initform :roman :reader slant)
   (spacing :initarg :spacing :initform :proportional :reader spacing)
   (weight :initarg :weight :initform :regular :reader weight)
   (stretch :initarg :stretch :initform :normal :reader stretch)))

(defmethod request-font ((renderer renderer) (family string) &rest initargs)
  (apply #'make-instance 'font :family family initargs))

(defclass image ()
  ((size :initarg :size :reader size)
   (data :initarg :data :reader data)))

(defmethod request-image ((renderer renderer) (data vector) &rest initargs)
  (apply #'make-instance 'image :data data initargs))

(defclass gradient ()
  ((start :initarg :start :initform (arg! :start) :reader start)
   (stop :initarg :stop :initform (arg! :stop) :reader stop)
   (stops :initarg :stops :initform (arg! :stops) :reader stops)))

(defclass linear-gradient (gradient) ())
(defclass radial-gradient (gradient) ())
(defclass angle-gradient (gradient) ())
(defclass diamond-gradient (gradient) ())

(defmethod request-gradient ((renderer renderer) type start stop stops &rest initargs)
  (apply #'make-instance type :start start :stop stop :stops stops initargs))

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

(defmethod rectangle ((renderer renderer) (bounds alloy:extent) &rest initargs &key line-width)
  (apply #'make-instance (if line-width 'outlined-rectangle 'filled-rectangle) :bounds bounds initargs))

(defclass ellipse (shape)
  ((bounds :initarg :bounds :initform (arg! :bounds) :accessor bounds)))

(defclass filled-ellipse (ellipse filled-shape) ())
(defclass outlined-ellipse (ellipse outlined-shape) ())

(defmethod ellipse ((renderer renderer) (bounds alloy:extent) &rest initargs &key line-width)
  (apply #'make-instance (if line-width 'outlined-ellipse 'filled-ellipse) :bounds bounds initargs))

(defclass polygon (filled-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod polygon ((renderer renderer) (points vector) &rest initargs)
  (apply #'make-instance 'outlined-polygon :points points initargs))

(defclass line-strip (outlined-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod line-strip ((renderer renderer) (points vector) &rest initargs)
  (apply #'make-instance 'line-strip :points points initargs))

(defclass curve (outlined-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod curve ((renderer renderer) (points vector) &rest initargs)
  (apply #'make-instance 'curve :points points initargs))

;; FIXME: changing styles and size, multiple styles and size per text
(defclass text (shape)
  ((alloy:text :initarg :text :initform (arg! :text) :accessor alloy:text)
   (font :initarg :font :initform (arg! :font) :accessor font)
   (color :initarg :color :initform colors:black :accessor color)
   (size :initarg :size :initform (arg! :size) :accessor size)
   (bounds :initarg :bounds :initform (alloy:margins) :accessor bounds)
   (valign :initarg :valign :initform :middle :accessor valign)
   (halign :initarg :halign :initform :start :accessor halign)
   (direction :initarg :direction :initform :right :accessor direction)))

(defmethod text ((renderer renderer) (bounds alloy:extent) (string string) &key (font (request-font renderer :default))
                                                                                (size (alloy:un 10))
                                                                                (color colors:black)
                                                                                (align '(:middle :left))
                                                                                (direction :right))
  (destructuring-bind (valign halign) align
    (make-instance 'text :text string :font font :size size :color color
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
