#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.glfw)

(defclass cursor (window:cursor)
  ((pointer :accessor pointer)
   (window :initarg :window :initform (alloy:arg! :window) :reader window)
   (icon :initform NIL :reader window:icon :writer set-icon)))

(defmethod window:locked-p ((cursor cursor))
  (eq :disabled (%glfw:get-input-mode :cursor (pointer (window cursor)))))

(defmethod (setf window:locked-p) (locked (cursor cursor))
  (%glfw:set-input-mode :cursor (if locked :disabled :normal) (pointer (window cursor)))
  locked)

(defmethod window:visible-p ((cursor cursor))
  (eq :normal (%glfw:get-input-mode :cursor (pointer (window cursor)))))

(defmethod (setf window:visible-p) (visible (cursor cursor))
  (%glfw:set-input-mode :cursor (if visible :normal :hidden) (pointer (window cursor)))
  visible)

(defmethod (setf window:icon) ((none null) (cursor cursor))
  (%glfw::set-cursor (pointer (window cursor)) (cffi:null-pointer))
  (slot-makunbound cursor 'pointer))

(defmethod (setf window:icon) ((type keyword) (cursor cursor))
  (let ((type (case type
                ((:default :text :crosshair :pointer :h-resize :v-resize) type)
                (T :default)))
        (pointer (%glfw::create-standard-cursor image 0 0)))
    (%glfw::set-cursor (pointer (window cursor)) pointer)
    (when (slot-boundp cursor 'pointer)
      (%glfw::destroy-cursor (pointer cursor)))
    (setf (pointer cursor) pointer)
    (set-icon type cursor)))

(defmethod (setf window:icon) ((icon icon) (cursor cursor))
  (cffi:with-foreign-object (image '(:struct %glfw::image))
    (cffi:with-foreign-array (data (simple:data icon) '(:uchar))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::width) (alloy:pxw (simple:size icon)))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::height) (alloy:pxh (simple:size icon)))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::data) data)
      (let ((pointer (%glfw::create-cursor image 0 0)))
        (%glfw::set-cursor (pointer (window cursor)) pointer)
        (when (slot-boundp cursor 'pointer)
          (%glfw::destroy-cursor (pointer cursor)))
        (setf (pointer cursor) pointer)
        (set-icon icon cursor)))))

(defclass monitor ()
  ((pointer :initarg :pointer :initform (alloy:arg! :pointer) :reader pointer)))

(defmethod window:size ((monitor monitor))
  (destructuring-bind (&key width height &allow-other-keys) (glfw:get-video-mode (pointer monitor))
    (alloy:px-size width height)))

(defclass ui (window:screen)
  ())

(defmethod window:list-monitors ((ui ui))
  (let ((primary (glfw:get-primary-monitor)))
    (list* (make-instance 'monitor :pointer primary)
           (loop for pointer in (glfw:get-monitors)
                 unless (cffi:pointer-eq primary pointer)
                 collect (make-instance 'monitor :pointer pointer)))))

(defmethod window:size ((ui ui))
  )

(defun call-with-ui (function)
  (cl-glfw3:initialize)
  (loop with ui = (make-instance 'ui :state :hidden)
        until (cl-glfw3:window-should-close-p (pointer ui))
        do (cl-glfw3:wait-events)
           (alloy:maybe-render ui T))
  (%glfw:terminate))

(defclass window (window:window renderer)
  ((cursor :reader window:cursor)
   (title :initarg :title :accessor window:title)
   (icon :initarg :icon :accessor window:icon)
   (min-size :initarg :min-size :accessor window:min-size)
   (max-size :initarg :max-size :accessor window:max-size)))

(defmethod make-window ((ui ui) &rest title (icon *default-window-icon*) (bounds *default-window-bounds*)
                                      monitor min-size max-size state decorated-p always-on-top-p
                                     &allow-other-keys)
  (let* ((window (make-instance 'window :parent ui
                                        :focus-parent (alloy:root (alloy:focus-tree ui))
                                        :layout-parent (alloy:root (alloy:layout-tree ui))
                                        :title (or title window:*default-window-title*)
                                        :size bounds
                                        :monitor monitor
                                        :visible-p (not (eq state :hidden))
                                        :decorated-p decorated-p))
         (pointer (pointer window)))
    (setf (slot-value window 'alloy:renderer) window)
    (when (typep bounds 'alloy:extent)
      (%glfw:set-window-position (alloy:pxx bounds) (alloy:pxy bounds) pointer))
    (%glfw:set-window-size-limits
     pointer
     (if min-size (alloy:pxw min-size) :dont-care) (if min-size (alloy:pxh min-size) :dont-care)
     (if max-size (alloy:pxw max-size) :dont-care) (if max-size (alloy:pxh max-size) :dont-care))
    (setf (window:icon window) icon)
    (setf (window:always-on-top-p window) always-on-top-p)
    (when state
      (setf (window:state window) state))
    (%glfw:set-window-position-callback pointer (cffi:callback window-position-callback))
    (%glfw:set-window-size-callback pointer (cffi:callback window-size-callback))
    (%glfw:set-window-close-callback pointer (cffi:callback window-close-callback))
    (%glfw:set-window-refresh-callback pointer (cffi:callback window-refresh-callback))
    (%glfw:set-window-focus-callback pointer (cffi:callback window-focus-callback))
    (%glfw:set-window-iconify-callback pointer (cffi:callback window-iconify-callback))
    (%glfw:set-window-maximize-callback pointer (cffi:callback window-maximize-callback))
    (%glfw:set-window-focus-callback pointer (cffi:callback window-focus-callback))
    (%glfw:set-mouse-button-callback pointer (cffi:callback mouse-button-callback))
    (%glfw:set-cursor-position-callback pointer (cffi:callback cursor-position-callback))
    (%glfw:set-cursor-enter-callback pointer (cffi:callback cursor-enter-callback))
    (%glfw:set-scroll-callback pointer (cffi:callback scroll-callback))
    (%glfw:set-key-callback pointer (cffi:callback key-callback))
    (%glfw:set-char-callback pointer (cffi:callback char-callback))))

(defmethod window:close ((window window))
  (alloy:deallocate window))

(defmethod (setf alloy:bounds) :before (bounds (window window))
  (destructuring-bind (x y) (%glfw:get-window-position (pointer window))
    (destructuring-bind (w h) (%glfw:get-window-size (pointer window))
      (unless (and (= x (alloy:pxx bounds)) (= y (alloy:pxy bounds)))
        (%glfw:set-window-position (pointer window) (alloy:pxx bounds) (alloy:pxy bounds)))
      (unless (and (= w (alloy:pxw bounds)) (= h (alloy:pxh bounds)))
        (%glfw:set-window-size (pointer window) (alloy:pxw bounds) (alloy:pxh bounds))))))

(defmethod window:notify ((window window))
  (%glw::request-window-attention (pointer window)))

(defmethod window:move-to-front ((window window))
  (%glfw::focus-window (pointer window)))

(defmethod window:move-to-back ((window window)))

(defmethod (setf window:max-size) :before (max-size (window window))
  (let ((min-size (window:min-size window)))
    (%glfw:set-window-size-limits
     (pointer window)
     (if min-size (alloy:pxw min-size) :dont-care) (if min-size (alloy:pxh min-size) :dont-care)
     (if max-size (alloy:pxw max-size) :dont-care) (if max-size (alloy:pxh max-size) :dont-care))))

(defmethod (setf window:min-size) :before (min-size (window window))
  (let ((max-size (window:max-size window)))
    (%glfw:set-window-size-limits
     (pointer window)
     (if min-size (alloy:pxw min-size) :dont-care) (if min-size (alloy:pxh min-size) :dont-care)
     (if max-size (alloy:pxw max-size) :dont-care) (if max-size (alloy:pxh max-size) :dont-care))))

(defmethod window:decorated-p ((window window))
  (%glfw:get-window-attribute :decorated (pointer window)))

(defmethod (setf window:decorated-p) (decorated (window (window window)))
  (%glfw::set-window-attrib (pointer window) :decorated decorated))

(defmethod (setf window:title) :before (title (window window))
  (%glfw:set-window-title title (pointer window)))

(defmethod (setf window:icon) :before (icon (window window))
  (cffi:with-foreign-object (image '(:struct %glfw::image))
    (cffi:with-foreign-array (data (simple:data icon) '(:uchar))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::width) (alloy:pxw (simple:size icon)))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::height) (alloy:pxh (simple:size icon)))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::data) data)
      (%glfw::set-window-icon (pointer window) 1 image))))

(defmethod window:always-on-top-p ((window window)) NIL)
(defmethod (setf window:always-on-top-p) (top (window window)) NIL)

(defmethod window:fullscreen ((window window) monitor)
  (destructuring-bind (&key width height refresh-rate &allow-other-keys) (glfw:get-video-mode (pointer monitor))
    (%glfw:set-window-monitor (pointer window) (pointer monitor)
                              0 0 width height refresh-rate)))

(defmethod window:state ((window window))
  (cond ((%glfw:get-window-attribute :iconified (pointer window)) :minimized)
        ((%glfw:get-window-attribute :maximized (pointer window)) :maximized)
        ((not (%glfw:get-window-attribute :visible (pointer window))) :hidden)
        ((not (cffi:null-pointer-p (%glfw:get-window-monitor (pointer window)))) :fullscreen)
        (T :normal)))

(defmethod (setf window:state) (state (window window))
  (ecase state
    (:minimized
     (unless (cffi:null-pointer-p (%glfw:get-window-monitor (pointer window)))
       (setf (window:state window) :normal))
     (%glfw:iconify-window (pointer window)))
    (:maximized
     (unless (cffi:null-pointer-p (%glfw:get-window-monitor (pointer window)))
       (setf (window:state window) :normal))
     (%glfw::maximize-window (pointer window)))
    (:hidden
     (unless (cffi:null-pointer-p (%glfw:get-window-monitor (pointer window)))
       (setf (window:state window) :normal))
     (%glfw:hide-window (pointer window)))
    (:normal
     (if (cffi:null-pointer-p (%glfw:get-window-monitor (pointer window)))
         (%glfw:restore-window (pointer window))
         (%glfw:set-window-monitor (pointer window) (cffi:null-pointer)
                                   (alloy:pxx (alloy:bounds window))
                                   (alloy:pxy (alloy:bounds window))
                                   (alloy:pxw (alloy:bounds window))
                                   (alloy:pxh (alloy:bounds window))
                                   0)))
    (:fullscreen
     (window:fullscreen window (first (list-monitors (parent window))))))
  state)

(defmacro define-callback (name args &body body)
  (destructuring-bind (window &rest args) args
    `(cffi:defcallback ,name :void ((,window :pointer) ,@args)
       (let ((,window (gethash ,window *window-map*)))
         (flet ((handle (ev)
                  (alloy:handle ev window window)))
           (when ,window
             ,@body))))))

(define-callback key-callback (window (key %glfw::key) (code :int) (action %glfw::key-action) (mods %glfw::mod-keys))
  (case action
    (:press
     (handle (make-instance 'alloy:key-down :code code :key key :modifiers mods)))
    (:release
     (handle (make-instance 'alloy:key-up :code code :key key :modifiers mods)))))

(define-callback scroll-callback (window (x :double) (y :double))
  (handle (make-instance 'alloy:scroll
                         :delta y
                         :location (cursor-location window))))

(define-callback char-callback (window (char :unsigned-int))
  (handle (make-instance 'alloy:text-event :text (string (code-char char)))))

(define-callback cursor-enter-callback (window (entered :boolean))
  (if entered
      (handle (make-instance 'window:pointer-enter :location (cursor-location window)))
      (handle (make-instance 'window:pointer-leave :location (cursor-location window)))))

(define-callback cursor-position-callback (window (x :double) (y :double))
  (let ((location (alloy:px-point x y)))
    (handle (make-instance 'alloy:pointer-move
                           :old-location (cursor-location window)
                           :location location))
    (setf (cursor-location window) location)))

(define-callback mouse-button-callback (window (button %glfw::mouse) (action %glfw::key-action) (mods %glfw::mod-keys))
  (case action
    (:press
     (handle (make-instance 'alloy:pointer-down :kind button)))
    (:release
     (handle (make-instance 'alloy:pointer-up :kind button)))))

(define-callback window-maximize-callback (window (maximize :boolean))
  (handle (make-instance 'window:state :new-state (if maximize :maximized (state window)))))

(define-callback window-iconify-callback (window (iconify :boolean))
  (handle (make-instance 'window:state :new-state (if maximize :minimized (state window)))))

(define-callback window-focus-callback (window (focused :boolean))
  (setf (alloy:focus window) (if focused
                                 (or (alloy:focus window) :strong)
                                 NIL)))

(define-callback window-refresh-callback (window)
  (alloy:render window T))

(define-callback window-close-callback (window)
  (handle (make-instance 'winodw:close)))

(define-callback window-size-callback (window (w :int) (h :int))
  (let ((bounds (alloy:bounds window)))
    (alloy:suggest-bounds (alloy:px-extent (alloy:pxx bounds) (alloy:pxy bounds) w h) window)))

(define-callback window-position-callback (window (x :int) (y :int))
  (let ((bounds (alloy:bounds window)))
    (alloy:suggest-bounds (alloy:px-extent x y (alloy:pxw bounds) (alloy:pxh bounds)) window)))
