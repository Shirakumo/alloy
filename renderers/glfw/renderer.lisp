(in-package #:org.shirakumo.alloy.renderers.glfw)

(defvar *window-map* (make-hash-table :test 'eq))

(defclass icon (window:icon simple:image)
  ())

(defclass renderer (org.shirakumo.alloy.renderers.opengl.msdf:renderer
                    org.shirakumo.alloy.renderers.opengl.png:renderer)
  ((parent :initarg :parent :accessor parent)
   (pointer :accessor pointer)))

(defmethod initialize-instance :after ((renderer renderer) &key title (size (alloy:px-size 1 1)) visible-p decorated-p (resizable-p t))
  (let ((glfw:*window* NIL))
    (%glfw:make-context-current (cffi:null-pointer))
    (float-features:with-float-traps-masked T
      (glfw:create-window
       :width (round (alloy:pxw size))
       :height (round (alloy:pxh size))
       :title (or title "")
       :visible visible-p
       :decorated decorated-p
       :resizable resizable-p
       :opengl-forward-compat T
       :opengl-profile :opengl-core-profile
       :context-version-major 3
       :context-version-minor 3
       :shared (if (slot-boundp renderer 'parent)
                   (pointer (parent renderer))
                   (cffi:null-pointer))))
    (gl:clear-color 0 0 0 0)
    (gl:enable :blend :depth-test :depth-clamp :stencil-test)
    (gl:clear-stencil #x00)
    (gl:stencil-func :always 1 #xFF)
    (gl:stencil-mask #xFF)
    (gl:clear-depth 0.0)
    (gl:depth-func :gequal)
    (gl:depth-mask T)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (setf (gethash (cffi:pointer-address glfw:*window*) *window-map*) renderer)
    (setf (pointer renderer) glfw:*window*)))

(defmethod alloy:allocate ((renderer renderer)))

(defmethod alloy:deallocate ((renderer renderer))
  (cl-glfw3:destroy-window (pointer renderer))
  (remhash (pointer renderer) *window-map*)
  (slot-makunbound renderer 'pointer))

(defmethod window:make-icon ((renderer renderer) size pixel-data)
  (make-instance 'icon :size size :data pixel-data))
