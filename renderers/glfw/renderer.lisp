(in-package #:org.shirakumo.alloy.renderers.glfw)

(defclass icon (window:icon simple:image)
  ())

(defclass renderer (org.shirakumo.alloy.renderers.opengl.msdf:renderer
                    org.shirakumo.alloy.renderers.opengl.png:renderer
                    glfw:window)
  ((parent :initarg :parent :accessor parent)))

(defmethod initialize-instance :around ((renderer renderer) &rest args &key (size (alloy:px-size 1 1)))
  (apply #'call-next-method renderer
         :width (round (alloy:pxw size))
         :height (round (alloy:pxh size))
         :opengl-forward-compat T
         :opengl-profile :opengl-core-profile
         :context-version-major 3
         :context-version-minor 3
         :shared (if (slot-boundp renderer 'parent)
                     (parent renderer)
                     (cffi:null-pointer))
         :allow-other-keys T
         :visible NIL
         args)
  (gl:clear-color 0 0 0 0)
  (gl:enable :blend :depth-test :stencil-test)
  (gl:clear-stencil #x00)
  (gl:stencil-func :always 1 #xFF)
  (gl:stencil-mask #xFF)
  (gl:clear-depth 0.0)
  (gl:depth-func :gequal)
  (gl:depth-mask T)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defmethod alloy:allocate ((renderer renderer)))

(defmethod alloy:deallocate ((renderer renderer))
  (glfw:destroy renderer))

(defmethod window:make-icon ((renderer renderer) size pixel-data)
  (make-instance 'icon :size size :data pixel-data))
