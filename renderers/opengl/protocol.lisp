#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.opengl)

;; Required GL state before ALLOY:RENDER call:
;;   (gl:enable :blend :depth-test :stencil-test)
;;   (gl:clear-stencil #x00)
;;   (gl:stencil-func :always 1 #xFF)
;;   (gl:stencil-mask #xFF)
;;   (gl:clear-depth 1.0)
;;   (gl:depth-func :lequal)
;;   (gl:depth-mask T)
;;   (gl:blend-func :src-alpha :one-minus-src-alpha)
;; If cull-face is enabled:
;;   (gl:front-face :ccw)
;;   (gl:cull-face :back)
;; The target being rendered to must have a color and a
;; combined depth-stencil attachment.

;; alloy:allocate
;; alloy:deallocate
;; simple:text
;; simple:request-font
;; simple:request-image

(defgeneric view-size (renderer))

(defgeneric bind (resource))
(defgeneric gl-name (resource))

(defgeneric make-shader (renderer &key vertex-shader fragment-shader))
(defgeneric (setf uniform) (value shader uniform))

(defgeneric make-vertex-buffer (renderer contents &key buffer-type data-usage))
(defgeneric update-vertex-buffer (buffer contents))

(defgeneric make-vertex-array (renderer bindings))
(defgeneric draw-vertex-array (array primitive-type offset count))

(defgeneric make-texture (renderer width height data &key channels filtering))

(defvar *gl-extensions* ())

(defun cache-gl-extensions ()
  (let ((*package* (find-package "KEYWORD")))
    (setf *gl-extensions*
          (loop for i from 0 below (gl:get* :num-extensions)
                for name = (ignore-errors (gl:get-string-i :extensions i))
                when name
                collect (cffi:translate-name-from-foreign name *package*)))))

(defmacro gl-extension-case (&body cases)
  (let ((extensions (gensym "EXTENSIONS")))
    `(let ((,extensions *gl-extensions*))
       (cond ,@(loop for (find . body) in cases
                     collect (case find
                               ((T otherwise)
                                `(T ,@body))
                               (T
                                `((and ,@(loop for extension in (if (listp find) find (list find))
                                               collect `(find ,extension ,extensions)))
                                  ,@body))))))))
