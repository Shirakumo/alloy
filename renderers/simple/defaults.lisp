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

(defmethod request-gradient ((renderer renderer) type start stop stops)
  (make-instance type :start start :stop stop :stops stops))

(defclass shape () ())
(defclass filled-shape (shape) ())
(defclass outlined-shape (shape) ())

(defclass rectangle (shape)
  ((extent :initarg :extent :initform (arg! :extent) :accessor extent)))

(defclass filled-rectangle (rectangle filled-shape) ())
(defclass outlined-rectangle (rectangle outlined-shape) ())

(defmethod rectangle ((renderer renderer) (extent alloy:extent) &key filled)
  (if filled
      (make-instance 'filled-rectangle :extent extent)
      (make-instance 'outlined-rectangle :extent extent)))

(defclass ellipse (shape)
  ((extent :initarg :extent :initform (arg! :extent) :accessor extent)))

(defclass filled-ellipse (ellipse filled-shape) ())
(defclass outlined-ellipse (ellipse outlined-shape) ())

(defmethod ellipse ((renderer renderer) (extent alloy:extent) &key filled)
  (if filled
      (make-instance 'filled-ellipse :extent extent)
      (make-instance 'outlined-ellipse :extent extent)))

(defclass polygon (shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defclass filled-polygon (polygon filled-shape) ())
(defclass outlined-polygon (polygon outlined-shape) ())

(defmethod polygon ((renderer renderer) (points vector) &key filled)
  (if filled
      (make-instance 'filled-polygon :points points)
      (make-instance 'outlined-polygon :points points)))

(defclass line-strip (outlined-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod line-strip ((renderer renderer) (points vector))
  (make-instance 'line-strip :points points))

(defclass curve (outlined-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod curve ((renderer renderer) (points vector))
  (make-instance 'curve :points poitns))

;; FIXME: changing styles and size, multiple styles and size per text
(defclass text (shape)
  ((text :initarg :text :initform (arg! :text) :accessor text)
   (font :initarg :font :initform (arg! :font) :accessor font)
   (size :initarg :size :initform (arg! :size) :accessor size)
   (extent :initarg :extent :initform (alloy:margins) :accessor extent)
   (valign :initarg :valign :initform :middle :accessor valign)
   (halign :initarg :halign :initform :start :accessor halign)
   (direction :initarg :direction :initform :right :accessor direction)))

(defmethod text ((renderer renderer) (point alloy:point) (string string) &key (font (request-font renderer :default))
                                                                              (size (alloy:un 10))
                                                                              (extent (alloy:margins))
                                                                              (align '(:middle :left))
                                                                              (direction :right))
  (destructuring-bind (valign halign) align
    (make-instance 'text :text text :font font :size size
                         :extent extent :direction direction
                         :valign valign :halign halign)))

(defclass icon (shape)
  ((image :initarg :image :initform (arg! :image) :accessor image)
   (size :initarg :size :initform (arg! :size) :accessor size)
   (extent :initarg :extent :initform (alloy:margins) :accessor extent)
   (valign :initarg :valign :initform :middle :accessor valign)
   (halign :initarg :halign :initform :left :accessor halign)))

(defmethod icon ((renderer renderer) (point alloy:point) (image image) &key (size (size image))
                                                                            (extent (alloy:margins))
                                                                            (align '(:middle :left)))
  (destructuring-bind (valign halign) align
    (make-instance 'icon :image image :size size :extent extent
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
