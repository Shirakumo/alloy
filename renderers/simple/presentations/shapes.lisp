#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

(defmethod to-extent ((shape shape) bounds)
  (to-extent (extent shape) bounds))

(defmethod to-extent (shape (element alloy:layout-element))
  (to-extent shape (alloy:bounds element)))

(defmethod to-extent ((extent alloy:extent) bounds)
  extent)

(defmethod to-extent ((margins alloy:margins) (bounds alloy:extent))
  ;; We ignore the bounds' x/y since we already have translated to that in local frame.
  (alloy:extent (alloy:margins-l margins)
                (alloy:margins-b margins)
                (- (alloy:extent-w bounds) (alloy:margins-l margins) (alloy:margins-r margins))
                (- (alloy:extent-h bounds) (alloy:margins-b margins) (alloy:margins-u margins))))

(defclass filled-shape (shape)
  ())

(defmethod render-sized :before ((renderer renderer) (shape filled-shape) extent)
  (setf (simple:fill-mode renderer) :fill))

(defclass outlined-shape (shape)
  ())

(defmethod render-sized :before ((renderer renderer) (shape outlined-shape) extent)
  (setf (simple:fill-mode renderer) :lines))

(defclass box (shape)
  ((extent :initarg :extent :initform (arg! :extent) :accessor extent)))

(defmethod render-sized ((renderer renderer) (box box) extent)
  (simple:rectangle renderer (to-extent box extent)))

(defclass filled-box (box filled-shape) ())
(defclass outlined-box (box outlined-shape) ())

(defclass circle (shape)
  ((extent :initarg :extent :initform (arg! :extent) :accessor extent)))

(defmethod render-sized ((renderer renderer) (circle circle) extent)
  (simple:ellipse renderer (to-extent circle extent)))

(defclass filled-circle (circle filled-shape) ())
(defclass outlined-circle (circle outlined-shape) ())

(defclass polygon (shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod render-sized ((renderer renderer) (polygon polygon) extent)
  (simple:polygon renderer (points polygon)))

(defclass filled-polygon (polygon filled-shape) ())
(defclass outlined-polygon (polygon outlined-shape) ())

(defclass line (shape)
  ((point-a :initarg :point-a :initform (arg! :point-a) :accessor point-a)
   (point-b :initarg :point-b :initform (arg! :point-b) :accessor point-b)))

(defmethod render-sized ((renderer renderer) (line line) extent)
  (simple:line renderer (point-a line) (point-b line)))

(defclass text (shape)
  ((text :initarg :text :initform (arg! :text) :accessor text)
   (extent :initarg :extent :initform (alloy:margins) :accessor extent)
   (valign :initarg :valign :initform :middle :accessor valign)
   (halign :initarg :halign :initform :start :accessor halign)
   (direction :initarg :direction :initform :right :accessor direction)))

(defmethod render-sized ((renderer renderer) (text text) extent)
  (let ((extent (to-extent text extent)))
    (simple:clip renderer extent)
    (let ((point (alloy:point (+ (alloy:extent-x extent)
                                 (ecase (halign text)
                                   (:start 0)
                                   (:middle (/ (alloy:extent-w extent) 2))
                                   (:end (alloy:extent-w extent))))
                              (+ (alloy:extent-y extent)
                                 (ecase (valign text)
                                   (:bottom 0)
                                   (:middle (/ (alloy:extent-h extent) 2))
                                   (:top (alloy:extent-h extent)))))))
      (simple:text renderer point (text text)
                   :direction (direction text)
                   :align (halign text)
                   :vertical-align (valign text)))))

(defclass icon (shape)
  ((image :initarg :image :initform (arg! :image) :accessor image)
   (extent :initarg :extent :initform (alloy:margins) :accessor extent)
   (valign :initarg :valign :initform :middle :accessor valign)
   (halign :initarg :halign :initform :left :accessor halign)
   (size :initarg :size :accessor size)))

(defmethod initialize-instance :after ((icon icon) &key image)
  (unless (slot-boundp icon 'size)
    (setf (size icon) (simple:size image))))

(defmethod render-sized ((renderer renderer) (icon icon) extent)
  (let ((extent (to-extent icon extent))
        (size (size icon)))
    (let ((point (alloy:point (+ (alloy:extent-x extent)
                                 (ecase (halign icon)
                                   (:start 0)
                                   (:middle (/ (- (alloy:extent-w extent) (alloy:size-w size)) 2))
                                   (:end (- (alloy:extent-w extent) (alloy:size-w size)))))
                              (+ (alloy:extent-y extent)
                                 (ecase (valign icon)
                                   (:bottom 0)
                                   (:middle (/ (- (alloy:extent-h extent) (alloy:size-h size)) 2))
                                   (:top (- (alloy:extent-h extent) (alloy:size-h size))))))))
      (simple:image renderer point (image icon) :size size))))

