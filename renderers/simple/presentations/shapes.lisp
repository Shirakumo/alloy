#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

(defmethod to-extent ((shape shape) bounds)
  (to-extent (extent shape) bounds))

(defmethod to-extent ((extent alloy:extent) (element alloy:layout-element))
  (alloy:px-extent (alloy:extent-x extent) (alloy:extent-y extent)
                   (alloy:extent-w extent) (alloy:extent-h extent)))

(defmethod to-extent ((margins alloy:margins) (element alloy:layout-element))
  (let ((bounds (alloy:bounds element)))
    ;; We ignore the bounds' x/y since we already have translated to that in local frame.
    (alloy:px-extent (alloy:pxl margins)
                     (alloy:pxb margins)
                     (- (alloy:pxw bounds)
                        (alloy:pxl margins)
                        (alloy:pxr margins))
                     (- (alloy:pxh bounds)
                        (alloy:pxb margins)
                        (alloy:pxu margins)))))

(defclass filled-shape (shape)
  ())

(defmethod render-inside :before ((renderer renderer) (shape filled-shape) renderable)
  (setf (simple:fill-mode renderer) :fill))

(defclass outlined-shape (shape)
  ())

(defmethod render-inside :before ((renderer renderer) (shape outlined-shape) renderable)
  (setf (simple:fill-mode renderer) :lines))

(defclass box (shape)
  ((extent :initarg :extent :initform (arg! :extent) :accessor extent)))

(defmethod render-inside ((renderer renderer) (box box) renderable)
  (simple:rectangle renderer (to-extent box renderable)))

(defclass filled-box (box filled-shape) ())
(defclass outlined-box (box outlined-shape) ())

(defclass circle (shape)
  ((extent :initarg :extent :initform (arg! :extent) :accessor extent)))

(defmethod render-inside ((renderer renderer) (circle circle) renderable)
  (simple:ellipse renderer (to-extent circle renderable)))

(defclass filled-circle (circle filled-shape) ())
(defclass outlined-circle (circle outlined-shape) ())

(defclass polygon (shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod render-inside ((renderer renderer) (polygon polygon) renderable)
  (simple:polygon renderer (points polygon)))

(defclass filled-polygon (polygon filled-shape) ())
(defclass outlined-polygon (polygon outlined-shape) ())

(defclass text (shape)
  ((text :initarg :text :initform (arg! :text) :accessor text)
   (extent :initarg :extent :initform (alloy:margins) :accessor extent)
   (valign :initarg :valign :initform :middle :accessor valign)
   (halign :initarg :halign :initform :start :accessor halign)
   (direction :initarg :direction :initform :right :accessor direction)))

(defmethod render-inside ((renderer renderer) (text text) renderable)
  (let ((extent (to-extent text renderable)))
    (simple:clip renderer extent)
    (let ((point (alloy:px-point
                  (+ (alloy:pxx extent)
                     (ecase (halign text)
                       (:start 0)
                       (:middle (/ (alloy:pxw extent) 2))
                       (:end (alloy:pxw extent))))
                  (+ (alloy:pxy extent)
                     (ecase (valign text)
                       (:bottom 0)
                       (:middle (/ (alloy:pxh extent) 2))
                       (:top (alloy:pxh extent)))))))
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

(defmethod render-inside ((renderer renderer) (icon icon) renderable)
  (let ((extent (to-extent icon renderable))
        (size (size icon)))
    (let ((point (alloy:point (alloy:u+ (alloy:extent-x extent)
                                        (ecase (halign icon)
                                          (:start 0)
                                          (:middle (/ (- (alloy:pxw extent) (alloy:pxw size)) 2))
                                          (:end (- (alloy:pxw extent) (alloy:pxw size)))))
                              (alloy:u+ (alloy:extent-y extent)
                                        (ecase (valign icon)
                                          (:bottom 0)
                                          (:middle (/ (- (alloy:pxh extent) (alloy:pxh size)) 2))
                                          (:top (- (alloy:pxh extent) (alloy:pxh size))))))))
      (simple:image renderer point (image icon) :size size))))

