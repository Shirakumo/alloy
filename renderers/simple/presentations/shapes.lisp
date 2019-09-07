#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

(defclass filled-shape ()
  ())

(defmethod render :before ((shape filled-shape) (renderer renderer))
  (setf (simple:fill-mode renderer) :fill))

(defclass outlined-shape ()
  ())

(defmethod render :before ((shape outlined-shape) (renderer renderer))
  (setf (simple:fill-mode renderer) :lines))

(defclass box ()
  ((extent)))

(defmethod render ((box box) (renderer renderer))
  (simple:rectangle renderer (extent box)))

(defclass filled-box (box filled-shape) ())
(defclass outlined-box (box outlined-shape) ())

(defclass circle ()
  ((extent)))

(defmethod render ((circle circle) (renderer renderer))
  (simple:ellipse renderer (extent circle)))

(defclass filled-circle (circle filled-shape) ())
(defclass outlined-circle (circle outlined-shape) ())

(defclass polygon ()
  ((points)))

(defmethod render ((polygon polygon) (renderer renderer))
  (simple:polygon renderer (points polygon)))

(defclass filled-polygon (polygon filled-shape) ())
(defclass outlined-polygon (polygon outlined-shape) ())

(defclass line ()
  ((point-a)
   (point-b)))

(defmethod render ((line line) (renderer renderer))
  (simple:line renderer (point-a line) (point-b line)))

(defclass text ()
  ((text :initarg :text :initform (error "TEXT required.") :accessor text)
   (point :initarg :point :initform (alloy:point 0 0) :accessor point)
   (valign :initarg :valign :initform :center :accessor valign)
   (halign :initarg :halign :initform :left :accessor halign)
   (direction :initarg :direction :initform :right :accessor direction)))

(defmethod render ((text text) (renderer renderer))
  (simple:text renderer (point text) (text text)
               :direction (direction text)
               :align (halign text)
               :vertical-align (valign text)))
