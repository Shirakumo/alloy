#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.glfw)

(defvar *window-map* (make-hash-table :test 'eq))

(cffi:defcenum %glfw::cursor-shape
  (:default #x00036001)
  (:text #x00036002)
  (:crosshair #x00036003)
  (:pointer #x00036004)
  (:h-resize #x00036005)
  (:v-resize #x00036006))

(cffi:defcstruct %glfw::image
  (%glfw::width :int)
  (%glfw::height :int)
  (%glfw::pixels :pointer))

(cffi:defcfun (%glfw::set-window-icon "glfwSetWindowIcon") :void
  (window :pointer)
  (count :int)
  (images :pointer))

(cffi:defcfun (%glfw::create-cursor "glfwCreateCursor") :void
  (image :pointer)
  (xhot :int)
  (yhot :int))

(cffi:defcfun (%glfw::create-standard-cursor "glfwCreateStandardCursor") :void
  (shape %glfw::cursor-shape))

(cffi:defcfun (%glfw::destroy-cursor "glfwDestroyCursor") :void
  (cursor :pointer))

(cffi:defcfun (%glfw::set-cursor "glfwSetCursor") :void
  (window :pointer)
  (cursor :pointer))

(cffi:defcfun (%glfw::request-window-attention "glfwRequestWindowAttention") :void
  (window :pointer))

(cffi:defcfun (%glfw::focus-window "glfwFocusWindow") :void
  (window :pointer))

(cffi:defcfun (%glfw::maximize-window "glfwMaximizeWindow") :void
  (window :pointer))

(cffi:defcfun (%glfw::set-window-attrib "glfwSetWindowAttrib") :void
  (window :pointer)
  (attrib %glfw::window-hint)
  (value :boolean))

(cffi:defcfun (%glfw::set-window-maximize-callback "glfwSetWindowMaximizeCallback") :pointer
  (window :pointer)
  (fun :pointer))

(defclass icon (window:icon simple:image)
  ())

(defclass renderer (alloy:renderer)
  ((parent :initarg :parent :accessor parent)
   (pointer :accessor pointer)))

(defmethod initialize-instance :after ((renderer renderer) &key title (size (alloy:px-size 1 1)) visible-p decorated-p)
  (let ((glfw:*window* NIL))
    (glfw:create-window
     :width (round (alloy:pxw size))
     :height (round (alloy:pxh size))
     :title (or title "")
     :visible visible-p
     :decorated decorated-p
     :opengl-forward-compat T
     :opengl-profile :opengl-core-profile
     :context-version-major 3
     :context-version-minor 3
     :shared (if (slot-boundp renderer 'parent)
                 (pointer (parent renderer))
                 (cffi:null-pointer)))
    (setf (gethash (cffi:pointer-address glfw:*window*) *window-map*) renderer)
    (setf (pointer renderer) glfw:*window*)))

(defmethod alloy:allocate ((renderer renderer)))

(defmethod alloy:deallocate ((renderer renderer))
  (cl-glfw3:destroy-window (pointer renderer))
  (remhash (pointer renderer) *window-map*)
  (slot-makunbound renderer 'pointer))

(defmethod window:make-icon ((renderer renderer) size pixel-data)
  (make-instance 'icon :size size :data pixel-data))
