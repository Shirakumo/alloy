#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.glfw)

;; Crap missing from cl-glfw3

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

(cffi:defcfun (%glfw::wait-events-timeout "glfwWaitEventsTimeout") :void
  (timeout :double))
