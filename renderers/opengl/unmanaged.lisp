#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.opengl)

(defmethod view-size ((renderer renderer))
  (destructuring-bind (x y w h) (gl:get-integer :viewport 4)
    (declare (ignore x y))
    (alloy:px-size w h)))

(defstruct (gl-resource (:copier NIL) (:predicate NIL))
  (name 0 :type (unsigned-byte 32)))

(defmethod gl-name ((resource gl-resource))
  (gl-resource-name resource))

(defstruct (vbo (:constructor make-vbo (name)) (:include gl-resource) (:copier NIL) (:predicate NIL)))

(defstruct (vao (:constructor make-vao (name)) (:include gl-resource) (:copier NIL) (:predicate NIL)))

(defstruct (program (:constructor make-program (name)) (:include gl-resource) (:copier NIL) (:predicate NIL)))

(defmethod bind ((program program))
  (gl:use-program (gl-resource-name program)))

(defstruct (texture (:constructor make-texture (name)) (:include gl-resource) (:copier NIL) (:predicate NIL)))

(defmethod bind ((texture texture))
  (gl:bind-texture :texture-2d (gl-resource-name texture)))

(defmethod make-shader ((renderer renderer) &key vertex-shader fragment-shader)
  (let ((vert (gl:create-shader :vertex-shader))
        (frag (gl:create-shader :fragment-shader))
        (prog (gl:create-program)))
    (flet ((make (name source)
             (gl:shader-source name source)
             (gl:compile-shader name)
             (unless (gl:get-shader name :compile-status)
               (error "Failed to compile: ~%~a~%Shader source:~%~a"
                      (gl:get-shader-info-log name) source))))
      (make vert vertex-shader)
      (make frag fragment-shader)
      (gl:attach-shader prog vert)
      (gl:attach-shader prog frag)
      (gl:link-program prog)
      (gl:detach-shader prog vert)
      (gl:detach-shader prog frag)
      (unless (gl:get-program prog :link-status)
        (error "Failed to link: ~%~a"
               (gl:get-program-info-log prog)))
      (make-program prog))))

(defmethod (setf uniform) (value (program program) uniform)
  (etypecase value
    (vector
     (gl:uniform-matrix uniform 3 value))
    (single-float
     (gl:uniformf uniform value))
    (simple:color
     (gl:uniformf uniform (simple:r value) (simple:g value) (simple:b value) (simple:a value)))
    (alloy:point
     (gl:uniformf uniform (alloy:pxx value) (alloy:pxy value)))
    (alloy:size
     (gl:uniformf uniform (alloy:pxw value) (alloy:pxh value))))
  value)

(defmethod make-vertex-buffer ((renderer renderer) contents &key data-usage)
  (let ((name (gl:gen-buffer)))
    (gl:bind-buffer :array-buffer name)
    (gl:buffer-data :array-buffer data-usage contents)
    (gl:bind-buffer :array-buffer 0)
    (make-vbo name)))

(defmethod update-vertex-buffer ((buffer vbo) contents)
  (gl:bind-buffer :array-buffer (gl-resource-name buffer))
  (gl:buffer-data :array-buffer :stream-draw contents)
  (gl:bind-buffer :array-buffer 0)
  buffer)

(defmethod make-vertex-array ((renderer renderer) bindings)
  (let ((name (gl:gen-vertex-array)))
    (gl:bind-vertex-array name)
    (loop for binding in bindings
          for i from 0
          do (destructuring-bind (buffer &key (size 3) (stride 0) (offset 0)) binding
               (gl:bind-buffer :array-buffer (gl-resource-name buffer))
               (gl:vertex-attrib-pointer i size :float NIL stride offset)
               (gl:enable-vertex-attrib-array i)))
    (gl:bind-vertex-array 0)
    (make-vao name)))

(defmethod draw-vertex-array ((array vao) primitive-type count)
  (gl:bind-vertex-array (gl-resource-name array))
  (gl:draw-arrays primitive-type 0 count)
  (gl:bind-vertex-array 0)
  array)

(defmethod simple:request-image ((renderer renderer) (image simple:image))
  (let ((name (gl:gen-texture))
        (w (floor (alloy:pxw (simple:size image))))
        (h (floor (alloy:pxh (simple:size image)))))
    (gl:bind-texture :texture-2d name)
    (%gl:tex-storage-2d :texture-2d 0 :rgba w h)
    (gl:tex-sub-image-2d :texture-2d 0 0 0 w h :rgba :unsigned-byte (simple:data image))
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-border)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-border)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:bind-texture :texture-2d 0)
    (make-texture name)))
