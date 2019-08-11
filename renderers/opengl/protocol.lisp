#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.opengl)

;; alloy:allocate
;; alloy:deallocate
(defgeneric bind (resource))

(defgeneric make-shader (renderer &key vertex-shader fragment-shader))
(defgeneric (setf uniform) (value shader uniform))

(defgeneric make-vertex-buffer (renderer contents &key buffer-usage))
(defgeneric update-vertex-buffer (buffer contents))

(defgeneric make-vertex-array (renderer bindings))
(defgeneric draw-vertex-array (array primitive-type count))
