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

(defclass filled-shape ()
  ())

(defmethod alloy:render-with :before ((shape filled-shape) element (renderer renderer))
  (setf (simple:fill-mode renderer) :fill))

(defclass outlined-shape ()
  ())

(defmethod alloy:render-with :before ((shape outlined-shape) element (renderer renderer))
  (setf (simple:fill-mode renderer) :lines))

(defclass box ()
  ((extent :initarg :extent :initform (error "EXTENT required") :accessor extent)))

(defmethod alloy:render-with ((box box) element (renderer renderer))
  (simple:rectangle renderer (to-extent box element)))

(defclass filled-box (box filled-shape) ())
(defclass outlined-box (box outlined-shape) ())

(defclass circle ()
  ((extent :initarg :extent :initform (error "EXTENT required") :accessor extent)))

(defmethod alloy:render-with ((circle circle) element (renderer renderer))
  (simple:ellipse renderer (to-extent circle element)))

(defclass filled-circle (circle filled-shape) ())
(defclass outlined-circle (circle outlined-shape) ())

(defclass polygon ()
  ((points :initarg :points :initform (error "POINTS required") :accessor points)))

(defmethod alloy:render-with ((polygon polygon) element (renderer renderer))
  (simple:polygon renderer (points polygon)))

(defclass filled-polygon (polygon filled-shape) ())
(defclass outlined-polygon (polygon outlined-shape) ())

(defclass line ()
  ((point-a :initarg :point-a :initform (error "POINT-A required") :accessor point-a)
   (point-b :initarg :point-b :initform (error "POINT-B required") :accessor point-b)))

(defmethod alloy:render-with ((line line) element (renderer renderer))
  (simple:line renderer (point-a line) (point-b line)))

(defclass text ()
  ((text :initarg :text :initform (error "TEXT required.") :accessor text)
   (extent :initarg :extent :initform (alloy:margins) :accessor extent)
   (valign :initarg :valign :initform :center :accessor valign)
   (halign :initarg :halign :initform :left :accessor halign)
   (direction :initarg :direction :initform :right :accessor direction)))

(defmethod alloy:render-with ((text text) element (renderer renderer))
  (let ((extent (to-extent text element)))
    (simple:clip renderer extent)
    (let ((point (alloy:point (+ (alloy:extent-x extent)
                                 (ecase (halign text)
                                   (:left 0)
                                   (:center (/ (alloy:extent-w extent) 2))
                                   (:right (alloy:extent-w extent))))
                              (+ (alloy:extent-y extent)
                                 (ecase (valign text)
                                   (:bottom 0)
                                   (:center (/ (alloy:extent-h extent) 2))
                                   (:top (alloy:extent-h extent)))))))
      (simple:text renderer point (text text)
                   :direction (direction text)
                   :align (halign text)
                   :vertical-align (valign text)))))
