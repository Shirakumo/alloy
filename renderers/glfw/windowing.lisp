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

(defmethod window:state ((cursor cursor))
  (case (%glfw:get-input-mode :cursor (pointer (window cursor)))
    (:disabled :locked)
    (:hidden :hidden)
    (:normal :normal)
    (T :unknown)))

(defmethod (setf window:state) (state (cursor cursor))
  (%glfw:set-input-mode :cursor
                        (ecase state
                          (:locked :disabled)
                          (:hidden :hidden)
                          (:normal :normal))
                        (pointer (window cursor)))
  state)

(defmethod (setf window:icon) ((none null) (cursor cursor))
  (%glfw::set-cursor (pointer (window cursor)) (cffi:null-pointer))
  (slot-makunbound cursor 'pointer))

(defmethod (setf window:icon) ((type symbol) (cursor cursor))
  (let ((type (case type
                ((:default :text :crosshair :pointer :h-resize :v-resize) type)
                (T :default)))
        (pointer (%glfw::create-standard-cursor type)))
    (%glfw::set-cursor (pointer (window cursor)) pointer)
    (when (slot-boundp cursor 'pointer)
      (%glfw::destroy-cursor (pointer cursor)))
    (setf (pointer cursor) pointer)
    (set-icon type cursor)))

(defmethod (setf window:icon) ((icon icon) (cursor cursor))
  (cffi:with-foreign-object (image '(:struct %glfw::image))
    (cffi:with-foreign-array (data (simple:data icon) (list :array :uchar (length (simple:data icon))))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::width) (floor (alloy:pxw (simple:size icon))))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::height) (floor (alloy:pxh (simple:size icon))))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::pixels) data)
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

(defclass screen (window:screen renderer
                  org.shirakumo.alloy.renderers.simple.presentations::default-look-and-feel)
  ())

(defmethod window:list-monitors ((screen screen))
  (let ((primary (glfw:get-primary-monitor)))
    (list* (make-instance 'monitor :pointer primary)
           (loop for pointer in (glfw:get-monitors)
                 unless (cffi:pointer-eq primary pointer)
                 collect (make-instance 'monitor :pointer pointer)))))

(defmethod window:close ((screen screen))
  (%glfw:set-window-should-close (pointer screen) T))

(defmethod window:size ((screen screen))
  )

(defun call-with-screen (function)
  (let (screen)
    (glfw:initialize)
    (%glfw:set-error-callback (cffi:callback glfw-error-callback))
    (unwind-protect
         (progn
           (setf screen (make-instance 'screen))
           (alloy:allocate screen)
           (funcall function screen)
           (loop until (glfw:window-should-close-p (pointer screen))
                 do (%glfw::wait-events-timeout 0.1d0)
                    (alloy:do-elements (window (alloy:root (alloy:layout-tree screen)))
                      (when (%glfw:window-should-close-p (pointer window))
                        (alloy:deallocate window)))
                    (when (= 0 (alloy:element-count (alloy:root (alloy:layout-tree screen))))
                      (window:close screen))
                    (alloy:render screen screen)))
      (when screen (alloy:deallocate screen))
      (%glfw:terminate))))

(defmacro with-screen (args &body init-body)
  (let ((init (gensym "INIT")))
    `(flet ((,init ,args
              ,@init-body))
       (call-with-screen #',init))))

(defclass window (window:window renderer
                  org.shirakumo.alloy.renderers.simple.presentations::default-look-and-feel)
  ((cursor :reader window:cursor)
   (title :initarg :title :accessor window:title)
   (icon :initarg :icon :accessor window:icon)
   (cursor-location :initform (alloy:px-point 0 0) :accessor cursor-location)
   (min-size :initarg :min-size :accessor window:min-size)
   (max-size :initarg :max-size :accessor window:max-size)
   (background-color :initarg :background-color :initform colors:black :accessor window:background-color)))

(defmethod window:make-window ((screen screen) &key (title window:*default-window-title*)
                                                    (icon window:*default-window-icon*)
                                                    (bounds window:*default-window-bounds*)
                                                    (decorated-p T) (state :normal)
                                                    (background-color colors:black)
                                                    (class 'window)
                                                    min-size max-size always-on-top-p
                                                    &allow-other-keys)
  (let* ((window (make-instance class :parent screen
                                      :focus-parent (alloy:root (alloy:focus-tree screen))
                                      :layout-parent (alloy:root (alloy:layout-tree screen))
                                      :title title
                                      :size bounds
                                      :decorated-p decorated-p
                                      :background-color background-color))
         (pointer (pointer window)))
    (when (typep bounds 'alloy:extent)
      (%glfw:set-window-position pointer (round (alloy:pxx bounds)) (round (alloy:pxy bounds))))
    (%glfw:set-window-size-limits
     pointer
     (if min-size (round (alloy:pxw min-size)) %glfw:+dont-care+) (if min-size (round (alloy:pxh min-size)) %glfw:+dont-care+)
     (if max-size (round (alloy:pxw max-size)) %glfw:+dont-care+) (if max-size (round (alloy:pxh max-size)) %glfw:+dont-care+))
    (setf (window:icon window) icon)
    (setf (window:always-on-top-p window) always-on-top-p)
    (unless (eql state :hidden)
      (%glfw:show-window pointer)
      (setf (window:state window) state))
    (%glfw:set-window-position-callback pointer (cffi:callback window-position-callback))
    (%glfw:set-window-size-callback pointer (cffi:callback window-size-callback))
    (%glfw:set-window-close-callback pointer (cffi:callback window-close-callback))
    (%glfw:set-window-refresh-callback pointer (cffi:callback window-refresh-callback))
    (%glfw:set-window-focus-callback pointer (cffi:callback window-focus-callback))
    (%glfw:set-window-iconify-callback pointer (cffi:callback window-iconify-callback))
    (%glfw::set-window-maximize-callback pointer (cffi:callback window-maximize-callback))
    (%glfw:set-window-focus-callback pointer (cffi:callback window-focus-callback))
    (%glfw:set-mouse-button-callback pointer (cffi:callback mouse-button-callback))
    (%glfw:set-cursor-position-callback pointer (cffi:callback cursor-position-callback))
    (%glfw:set-cursor-enter-callback pointer (cffi:callback cursor-enter-callback))
    (%glfw:set-scroll-callback pointer (cffi:callback scroll-callback))
    (%glfw:set-key-callback pointer (cffi:callback key-callback))
    (%glfw:set-char-callback pointer (cffi:callback char-callback))
    (%glfw::set-drop-callback pointer (cffi:callback drop-callback))
    (alloy:allocate window)
    (alloy:render screen window)
    window))

(defmethod window:close ((window window))
  (%glfw:set-window-should-close (pointer window) T))

(defmethod alloy:register ((window window) (screen screen)))

(defmethod alloy:deallocate :before ((window window))
  (alloy:leave window (alloy:layout-parent window)))

(defmethod alloy:render ((screen screen) (window window))
  (%glfw:make-context-current (cffi:null-pointer))
  (%glfw:make-context-current (pointer window))
  (gl:clear-color (colored:red (window:background-color window))
                  (colored:green (window:background-color window))
                  (colored:blue (window:background-color window))
                  (colored:alpha (window:background-color window)))
  (gl:clear :color-buffer :depth-buffer :stencil-buffer)
  (destructuring-bind (w h) (%glfw:get-window-size (pointer window))
    (gl:viewport 0 0 w h))
  (when (window:layout-element window)
    (alloy:render window (window:layout-element window)))
  (%glfw:swap-buffers (pointer window)))

(defmethod alloy:maybe-render ((screen screen) (window window))
  (%glfw:make-context-current (cffi:null-pointer))
  (%glfw:make-context-current (pointer window))
  (destructuring-bind (w h) (%glfw:get-window-size (pointer window))
    (gl:viewport 0 0 w h))
  (when (window:layout-element window)
    (alloy:maybe-render window (window:layout-element window)))
  (%glfw:swap-buffers (pointer window)))

(defmethod alloy:suggest-bounds (bounds (window window))
  (destructuring-bind (x y) (%glfw:get-window-position (pointer window))
    (destructuring-bind (w h) (%glfw:get-window-size (pointer window))
      (unless (and (= x (alloy:pxx bounds)) (= y (alloy:pxy bounds)))
        (%glfw:set-window-position (pointer window) (round (alloy:pxx bounds)) (round (alloy:pxy bounds))))
      (unless (and (= w (alloy:pxw bounds)) (= h (alloy:pxh bounds)))
        (%glfw:set-window-size (pointer window) (max 1 (round (alloy:pxw bounds))) (max 1 (round (alloy:pxh bounds))))))))

(defmethod (setf alloy:bounds) :after (extent (window window))
  (let ((target (simple:transform-matrix window)))
    (setf (aref target 0) (/ 2f0 (max 1f0 (alloy:pxw extent))))
    (setf (aref target 1) 0f0)
    (setf (aref target 2) -1f0)
    
    (setf (aref target 3) 0f0)
    (setf (aref target 4) (/ 2f0 (max 1f0 (alloy:pxh extent))))
    (setf (aref target 5) -1f0)

    (setf (aref target 6) 0f0)
    (setf (aref target 7) 0f0)
    (setf (aref target 8) 0.0001f0)))

(defmethod alloy:dots-per-cm ((window window))
  (alloy:dots-per-cm (parent window)))

(defmethod alloy:target-resolution ((window window))
  (alloy:target-resolution (parent window)))

(defmethod alloy:resolution-scale ((window window))
  (alloy:resolution-scale (parent window)))

(defmethod alloy:base-scale ((window window))
  (alloy:base-scale (parent window)))

(defmethod window:notify ((window window))
  (%glfw::request-window-attention (pointer window)))

(defmethod window:move-to-front ((window window))
  (%glfw::focus-window (pointer window)))

(defmethod window:move-to-back ((window window)))

(defun set-window-size-limits (window min-size max-size)
  (%glfw:set-window-size-limits
   (pointer window)
   (if min-size (max 1 (round (alloy:pxw min-size))) %glfw:+dont-care+)
   (if min-size (max 1 (round (alloy:pxh min-size))) %glfw:+dont-care+)
   (if max-size (max 1 (round (alloy:pxw max-size))) %glfw:+dont-care+)
   (if max-size (max 1 (round (alloy:pxh max-size))) %glfw:+dont-care+)))

(defmethod (setf window:max-size) :before (max-size (window window))
  (set-window-size-limits window (window:min-size window) max-size))

(defmethod (setf window:min-size) :before (min-size (window window))
  (set-window-size-limits window min-size (window:max-size window)))

(defmethod window:decorated-p ((window window))
  (%glfw:get-window-attribute :decorated (pointer window)))

(defmethod (setf window:decorated-p) (decorated (window window))
  (%glfw::set-window-attrib (pointer window) :decorated decorated))

(defmethod (setf window:title) :before (title (window window))
  (%glfw:set-window-title title (pointer window)))

(defmethod (setf window:icon) :before ((icon icon) (window window))
  (cffi:with-foreign-object (image '(:struct %glfw::image))
    (cffi:with-foreign-array (data (simple:data icon) (list :array :uchar (length (simple:data icon))))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::width) (floor (alloy:pxw (simple:size icon))))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::height) (floor (alloy:pxh (simple:size icon))))
      (setf (cffi:foreign-slot-value image '(:struct %glfw::image) '%glfw::pixels) data)
      (%glfw::set-window-icon (pointer window) 1 image))))

(defmethod (setf window:icon) :before ((null null) (window window))
  (%glfw::set-window-icon (pointer window) 0 (cffi:null-pointer)))

(defmethod window:always-on-top-p ((window window))
  (%glfw:get-window-attribute (pointer window) #x00020007))

(defmethod (setf window:always-on-top-p) (top (window window))
  (%glfw::set-window-attrib (pointer window) #x00020007 top)
  top)

(defmethod window:fullscreen ((window window) monitor)
  (destructuring-bind (&key width height refresh-rate &allow-other-keys) (glfw:get-video-mode (pointer monitor))
    (%glfw:set-window-monitor (pointer window) (pointer monitor)
                              0 0 width height refresh-rate)))

(defmethod window:state ((window window))
  (cond ((%glfw:get-window-attribute (pointer window) :iconified) :minimized)
        ((%glfw:get-window-attribute (pointer window) :maximized) :maximized)
        ((not (%glfw:get-window-attribute (pointer window) :visible)) :hidden)
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
     (unless (%glfw:get-window-attribute (pointer window) :visible)
       (%glfw:show-window (pointer window)))
     (if (cffi:null-pointer-p (%glfw:get-window-monitor (pointer window)))
         (%glfw:restore-window (pointer window))
         (%glfw:set-window-monitor (pointer window) (cffi:null-pointer)
                                   (alloy:pxx (alloy:bounds window))
                                   (alloy:pxy (alloy:bounds window))
                                   (alloy:pxw (alloy:bounds window))
                                   (alloy:pxh (alloy:bounds window))
                                   0)))
    (:fullscreen
     (window:fullscreen window (first (window:list-monitors (parent window))))))
  state)

(defun handle-window-event (window ev)
  (if (typep ev 'alloy:pointer-event)
      (or (alloy:handle ev (alloy:focus-tree (parent window)))
          (alloy:handle ev window))
      (alloy:handle ev (parent window))))

(defmacro define-callback (name args &body body)
  (destructuring-bind (window &rest args) args
    `(progn
       (cffi:defcallback ,name :void ((,window :pointer) ,@args)
         (let ((,window (gethash (cffi:pointer-address ,window) *window-map*)))
           (cond (,window
                  (with-simple-restart (abort "Abort the callback.")
                    (,name ,window ,@(mapcar #'car args))))
                 (T
                  (format *error-output* "~&[GLFW] Callback ~a on unknown window." ',name)))))
       (defun ,name (,window ,@(mapcar #'car args))
         (flet ((handle (ev)
                  (handle-window-event ,window ev)))
           (declare (ignore #'handle))
           ,@body)))))

(cffi:defcallback glfw-error-callback :void ((code :int) (message :string))
  (format *error-output* "~&[GLFW] Error [~d]: ~a~%" code message)
  (finish-output *error-output*))

(define-callback key-callback (window (key %glfw::key) (code :int) (action %glfw::key-action) (mods %glfw::mod-keys))
  (case action
    (:press
     (handle (make-instance 'alloy:key-down :code code :key key :modifiers mods)))
    (:release
     (handle (make-instance 'alloy:key-up :code code :key key :modifiers mods)))))

(define-callback scroll-callback (window (x :double) (y :double))
  (handle (make-instance 'alloy:scroll
                         :dy y
                         :dx x
                         :location (cursor-location window))))

(define-callback char-callback (window (char :unsigned-int))
  (handle (make-instance 'alloy:text-event :text (string (code-char char)))))

(define-callback cursor-enter-callback (window (entered :boolean))
  (if entered
      (handle (make-instance 'window:pointer-enter :location (cursor-location window)))
      (handle (make-instance 'window:pointer-leave :location (cursor-location window)))))

(define-callback cursor-position-callback (window (x :double) (y :double))
  (let ((location (alloy:px-point x (- (second (%glfw:get-window-size (pointer window))) y))))
    (handle (make-instance 'alloy:pointer-move
                           :old-location (cursor-location window)
                           :location location))
    (setf (cursor-location window) location)))

(define-callback mouse-button-callback (window (button %glfw::mouse) (action %glfw::key-action) (mods %glfw::mod-keys))
  (case action
    (:press
     (handle (make-instance 'alloy:pointer-down
                            :location (cursor-location window)
                            :kind button)))
    (:release
     (handle (make-instance 'alloy:pointer-up 
                            :location (cursor-location window)
                            :kind button)))))

(define-callback window-maximize-callback (window (maximize :boolean))
  (handle (make-instance 'window:state :new-state (if maximize :maximized (window:state window)))))

(define-callback window-iconify-callback (window (iconify :boolean))
  (handle (make-instance 'window:state :new-state (if iconify :minimized (window:state window)))))

(define-callback window-focus-callback (window (focused :boolean))
  (setf (alloy:focus window) (if focused
                                 (or (alloy:focus window) :strong)
                                 NIL)))

(define-callback window-refresh-callback (window)
  (alloy:mark-for-render window))

(define-callback window-close-callback (window)
  (handle (make-instance 'window:close)))

(define-callback window-size-callback (window (w :int) (h :int))
  (let ((bounds (alloy:bounds window)))
    (setf (alloy:bounds window) (alloy:px-extent (alloy:pxx bounds) (alloy:pxy bounds) w h))))

(define-callback window-position-callback (window (x :int) (y :int))
  (let ((bounds (alloy:bounds window)))
    (setf (alloy:bounds window) (alloy:px-extent x y (alloy:pxw bounds) (alloy:pxh bounds)))))

(define-callback drop-callback (window (count :int) (paths :pointer))
  (let ((paths (loop for i from 0 below count
                     collect (cffi:mem-aref paths :string i))))
    (handle (make-instance 'alloy:drop-event :paths paths))))
