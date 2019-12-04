#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.opengl)

(defclass gradient (simple:gradient)
  ((data :reader data)))

(defmethod shared-initialize :after ((gradient gradient) slots &key)
  (setf (slot-value gradient 'data) (compute-gradient-data gradient)))

(defmethod simple:request-gradient ((renderer renderer) type start stop stops &key)
  (make-instance (find-symbol (string type) #.*package*) :start start :stop stop :stops stops))

(defmethod alloy:render ((renderer renderer) (shape gradient))
  (let ((shader (resource 'gradient-shader renderer))
        (data (data shape)))
    (update-vertex-buffer (resource 'gradient-vbo renderer) data)
    (bind shader)
    (setf (uniform shader "transform") (simple:transform-matrix renderer))
    (draw-vertex-array (resource 'gradient-vao renderer) :triangles (/ (length data) 6))))

(defmethod alloy:render :around ((renderer renderer) (shape simple:patterned-shape))
  (let ((pattern (simple:pattern shape)))
    (etypecase pattern
      (colored:color
       (call-next-method))
      (gradient
       ;; KLUDGE: we stub out the shape with an opaque colour to do the clipping
       ;;         then set it back. Not the nicest to modify the shape during render.
       (simple:with-pushed-transforms (renderer)
         (setf (simple:pattern shape) colors:black)
         (unwind-protect
              (simple:clip renderer shape)
           (setf (simple:pattern shape) pattern))
         (alloy:render renderer pattern))))))

(defgeneric compute-gradient-data (gradient))

;; FIXME: gradients
(defclass linear-gradient (gradient) ())

(defmethod compute-gradient-data ((gradient linear-gradient))
  )

(defclass radial-gradient (gradient) ())

(defmethod compute-gradient-data ((gradient radial-gradient))
  )

(defclass angle-gradient (gradient) ())

(defmethod compute-gradient-data ((gradient angle-gradient))
  )

(defclass diamond-gradient (gradient) ())

(defmethod compute-gradient-data ((gradient diamond-gradient))
  )
