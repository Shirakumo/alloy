#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.glfw)

(defclass image (simple:image)
  ())

(defclass renderer (alloy:renderer)
  ())

(defmethod alloy:allocate ((renderer renderer)))
(defmethod alloy:deallocate ((renderer renderer)))
(defmethod simple:request-image ((renderer renderer) (image image)))
(defmethod opengl:view-size ((renderer renderer)))
(defmethod opengl:bind (resource))
(defmethod opengl:gl-name (resource))
(defmethod opengl:make-shader ((renderer renderer) &key vertex-shader fragment-shader))
(defmethod (setf opengl:uniform) (value shader uniform))
(defmethod opengl:make-vertex-buffer ((renderer renderer) contents &key data-usage buffer-type))
(defmethod opengl:update-vertex-buffer (buffer contents))
(defmethod opengl:make-vertex-array ((renderer renderer) bindings))
(defmethod opengl:draw-vertex-array (array primitive-type count))
