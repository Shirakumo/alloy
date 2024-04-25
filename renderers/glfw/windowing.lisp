(in-package #:org.shirakumo.alloy.renderers.glfw)

(defclass cursor (window:cursor)
  ((window :initarg :window :initform (alloy:arg! :window) :reader window)
   (icon :initform NIL :reader window:icon :writer set-icon)))

(defmethod window:state ((cursor cursor))
  (case (glfw:input-mode :cursor (window cursor))
    (:disabled :locked)
    (:hidden :hidden)
    (:normal :normal)
    (T :unknown)))

(defmethod (setf window:state) (state (cursor cursor))
  (setf (glfw:input-mode :cursor (window cursor))
        (ecase state
          (:locked :disabled)
          (:hidden :hidden)
          (:normal :normal)))
  state)

(defmethod (setf window:icon) ((none null) (cursor cursor))
  (setf (glfw:cursor (window cursor)) NIL))

(defmethod (setf window:icon) ((type symbol) (cursor cursor))
  (let ((type (case type
                ((:default :crosshair) type)
                ((:text :ibeam) :ibeam)
                ((:pointer :pointing-hand) :pointing-hand)
                ((:h-resize :ew-resize :resize-ew) :resize-ew)
                ((:v-resize :ns-resize :resize-ns) :resize-ns)
                ((:nwse-resize :resize-nwse) :resize-nwse)
                ((:nesw-resize :resize-nesw) :resize-nesw)
                ((:resize :resize-all) :resize)
                ((:deny :not-allowed) :not-allowed)
                (T :default))))
    (setf (glfw:cursor (window cursor)) type)
    (set-icon type cursor)))

(defmethod (setf window:icon) ((icon icon) (cursor cursor))
  (let ((%cursor (make-instance 'glfw:cursor :pixels (simple:data icon)
                                             :width (floor (alloy:pxw (simple:size icon)))
                                             :height (floor (alloy:pxh (simple:size icon))))))
    (setf (glfw:cursor (window cursor)) %cursor)
    (set-icon icon cursor)))

(defmethod window:size ((monitor glfw:monitor))
  (let ((size (glfw:size monitor)))
    (alloy:px-size (first size) (second size))))

(defmethod window:location ((monitor glfw:monitor))
  (let ((location (glfw:location monitor)))
    (alloy:px-point (first location) (second location))))

(defclass screen (window:screen renderer
                  org.shirakumo.alloy.renderers.simple.presentations::default-look-and-feel)
  ())

(defmethod window:list-monitors ((screen screen))
  (glfw:list-monitors))

(defmethod window:close ((screen screen))
  (setf (glfw:should-close-p screen) T))

(defmethod window:size ((screen screen))
  )

(defun call-with-screen (function &optional (screen-class 'screen) &rest initargs)
  (let (screen
        (start (get-internal-real-time)))
    (glfw:init)
    (unwind-protect
         (progn
           (setf screen (apply #'make-instance screen-class initargs))
           (alloy:allocate screen)
           (funcall function screen)
           (loop until (glfw:should-close-p screen)
                 do (glfw:poll-events :timeout (float 1/30 0d0))
                    (alloy:do-elements (window (alloy:root (alloy:layout-tree screen)))
                      (when (glfw:should-close-p window)
                        (alloy:deallocate window)))
                    (when (= 0 (alloy:element-count (alloy:root (alloy:layout-tree screen))))
                      (window:close screen))
                    (let ((dt (float (/ (- (get-internal-real-time) start) INTERNAL-TIME-UNITS-PER-SECOND) 0f0)))
                      (org.shirakumo.alloy.animation:update screen dt)
                      (setf start (get-internal-real-time)))
                    (alloy:render screen screen)))
      (when screen (alloy:deallocate screen))
      (glfw:shutdown))))

(defmacro with-screen ((screen &rest args) &body init-body)
  (let ((init (gensym "INIT")))
    `(flet ((,init (,screen)
              ,@init-body))
       (call-with-screen #',init ,@args))))

(defmethod alloy:cursor ((screen screen))
  :default)

(defmethod (setf alloy:cursor) (value (screen screen))
  value)

(defclass window (window:window
                  renderer
                  org.shirakumo.alloy.renderers.simple.presentations::default-look-and-feel)
  ((cursor :reader window:cursor)
   (icon :initarg :icon :accessor window:icon)
   (cursor-location :initform (alloy:px-point 0 0) :accessor cursor-location)
   (background-color :initarg :background-color :initform colors:black :accessor window:background-color)))

(defmethod initialize-instance :after ((window window) &key)
  (setf (slot-value window 'cursor) (make-instance 'cursor :window window)))

(defmethod window:make-window ((screen screen) &key (title window:*default-window-title*)
                                                    (icon window:*default-window-icon*)
                                                    (bounds window:*default-window-bounds*)
                                                    (decorated-p T) (state :normal)
                                                    (resizable-p T)
                                                    (background-color colors:black)
                                                    (class 'window)
                                                    min-size max-size preferred-size always-on-top-p
                                                    &allow-other-keys)
  (let ((window (make-instance class :parent screen
                                     :focus-parent (alloy:root (alloy:focus-tree screen))
                                     :layout-parent (alloy:root (alloy:layout-tree screen))
                                     :title title
                                     :size bounds
                                     :decorated decorated-p
                                     :resizable resizable-p
                                     :background-color background-color)))
    (when (typep bounds 'alloy:extent)
      (setf (glfw:location window) (list (round (alloy:pxx bounds)) (round (alloy:pxy bounds)))))
    (when min-size (setf (window:min-size window) min-size))
    (when max-size (setf (window:max-size window) max-size))
    (when icon
      (setf (window:icon window) icon))
    (setf (window:always-on-top-p window) always-on-top-p)
    (unless (eql state :hidden)
      (glfw:show window)
      (setf (window:state window) state)
      (when preferred-size
        (alloy:suggest-size preferred-size window)))
    (alloy:allocate window)
    (alloy:render screen window)
    window))

(defmethod window:close ((window window))
  (setf (glfw:should-close-p window) T))

(defmethod animation:update ((window window) dt)
  (animation:update (window:layout-element window) dt))

(defmethod alloy:register ((window window) (screen screen)))

(defmethod alloy:deallocate :before ((window window))
  (alloy:leave window (alloy:layout-parent window)))

(defmethod alloy:render ((screen screen) (window window))
  (glfw:make-current NIL)
  (glfw:make-current window)
  (gl:clear-color (colored:red (window:background-color window))
                  (colored:green (window:background-color window))
                  (colored:blue (window:background-color window))
                  (colored:alpha (window:background-color window)))
  (gl:clear :color-buffer :depth-buffer :stencil-buffer)
  (destructuring-bind (w h) (glfw:framebuffer-size window)
    (gl:viewport 0 0 w h))
  (when (window:layout-element window)
    (alloy:render window (window:layout-element window)))
  (glfw:swap-buffers window))

(defmethod alloy:maybe-render ((screen screen) (window window))
  (glfw:make-current NIL)
  (glfw:make-current window)
  (destructuring-bind (w h) (glfw:framebuffer-size window)
    (gl:viewport 0 0 w h))
  (when (window:layout-element window)
    (alloy:maybe-render window (window:layout-element window)))
  ;; FIXME: only do this if anything was actually done.
  (glfw:swap-buffers window))

(defmethod window:title ((window window))
  (glfw:title window))

(defmethod (setf window:title) (value (window window))
  (setf (glfw:title window) value))

(defmethod window:min-size ((window window))
  (let ((limits (glfw:size-limits window)))
    (alloy:px-size (first limits) (second limits))))

(defmethod (setf window:min-size) (value (window window))
  (let ((limits (glfw:size-limits window)))
    (setf (glfw:size-limits window) (list (when value (round (alloy:pxw value)))
                                          (when value (round (alloy:pxw value)))
                                          (third limits)
                                          (fourth limits)))
    value))

(defmethod window:max-size ((window window))
  (let ((limits (glfw:size-limits window)))
    (alloy:px-size (first limits) (second limits))))

(defmethod (setf window:max-size) (value (window window))
  (let ((limits (glfw:size-limits window)))
    (setf (glfw:size-limits window) (list (first limits)
                                          (second limits)
                                          (when value (round (alloy:pxw value)))
                                          (when value (round (alloy:pxw value)))))
    value))

(defmethod alloy:suggest-size (size (window window))
  (setf (glfw:size window) (list (round (alloy:pxw size)) (round (alloy:pxh size)))))

(defmethod (setf alloy:location) :after (location (window window))
  (setf (glfw:location window) (list (round (alloy:pxx location)) (round (alloy:pxy location)))))

(defmethod (setf alloy:bounds) :after (extent (window window))
  (let ((target (simple:transform-matrix window)))
    (setf (simple:identity-matrix window) target)
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

(defmethod alloy:cursor ((window window))
  (or (window:icon window)
      :default))

(defmethod (setf alloy:cursor) (value (window window))
  (setf (window:icon (window:cursor window)) value))

(defmethod alloy:key-text (key (window window))
  (%glfw:get-key-name key 0))

(defmethod alloy:key-text (key (screen screen))
  (%glfw:get-key-name key 0))

(defmethod window:notify ((window window))
  (glfw:request-attention window))

(defmethod window:move-to-front ((window window))
  (glfw:focus window))

(defmethod window:move-to-back ((window window)))

(defun get-window-bool-attribute (attribute window)
  (< 1 (glfw:attribute attribute window)))

(defmethod window:decorated-p ((window window))
  (get-window-bool-attribute :decorated window))

(defmethod (setf window:decorated-p) (decorated (window window))
  (setf (glfw:attribute :decorated window) decorated))

(defmethod window:always-on-top-p ((window window))
  (get-window-bool-attribute :floating window))

(defmethod (setf window:always-on-top-p) (top (window window))
  (ignore-errors
   (setf (glfw:attribute :floating window) top))
  top)

(defmethod (setf window:icon) :before ((icon icon) (window window))
  (ignore-errors
   (setf (glfw:icon window) (list (list (simple:data icon)
                                        (floor (alloy:pxw (simple:size icon)))
                                        (floor (alloy:pxh (simple:size icon))))))))

(defmethod (setf window:icon) :before ((null null) (window window))
  (ignore-errors
   (setf (glfw:icon window) NIL)))

(defmethod window:fullscreen ((window window) monitor)
  (setf (glfw:monitor window) monitor))

(defmethod window:state ((window window))
  (let ((state (glfw:state window)))
    (case state
      (:iconified :minimized)
      (T state))))

(defmethod (setf window:state) (state (window window))
  (setf (glfw:state window)
        (case state
          (:minimized :iconified)
          (T state)))
  state)

(defun handle (window ev)
  (if (typep ev '(or alloy:pointer-event window:window-event))
      (or (alloy:handle ev (alloy:focus-tree (parent window)))
          (alloy:handle ev window))
      (alloy:handle ev (parent window))))

(defmethod glfw:key-changed ((window window) key code action mods)
  (case action
    (:press
     (handle window (make-instance 'alloy:key-down :code code :key key :modifiers mods)))
    (:repeat
     (handle window (make-instance 'alloy:key-down :code code :key key :modifiers mods :repeat-p T)))
    (:release
     (handle window (make-instance 'alloy:key-up :code code :key key :modifiers mods)))))

(defmethod glfw:mouse-scrolled ((window window) x y)
  (handle window (make-instance 'alloy:scroll
                                :dy y
                                :dx x
                                :location (cursor-location window))))

(defmethod glfw:char-entered ((window window) char)
  (handle window (make-instance 'alloy:text-event :text (string (code-char char)))))

(defmethod glfw:mouse-entered ((window window) entered)
  (if entered
      (handle window (make-instance 'window:pointer-enter :location (cursor-location window)))
      (handle window (make-instance 'window:pointer-leave :location (cursor-location window)))))

(defmethod glfw:mouse-moved ((window window) x y)
  (let ((location (alloy:px-point x (- (second (glfw:size window)) y))))
    (handle window (make-instance 'alloy:pointer-move
                                  :old-location (cursor-location window)
                                  :location location))
    (setf (cursor-location window) location)))

(defmethod glfw:mouse-button-changed ((window window) button action mods)
  (case action
    (:press
     (handle window (make-instance 'alloy:pointer-down
                                   :location (cursor-location window)
                                   :kind button)))
    (:release
     (handle window (make-instance 'alloy:pointer-up
                                   :location (cursor-location window)
                                   :kind button)))))

(defmethod glfw:window-maximized ((window window) maximize)
  (handle window (make-instance 'window:state :new-state (if maximize :maximized (window:state window)))))

(defmethod glfw:window-iconified ((window window) iconify)
  (handle window (make-instance 'window:state :new-state (if iconify :minimized (window:state window)))))

(defmethod glfw:window-focused ((window window) focused)
  (setf (alloy:focus window) (if focused
                                 (or (alloy:focus window) :strong)
                                 NIL)))

(defmethod glfw:window-refreshed ((window window))
  (alloy:mark-for-render window))

(defmethod glfw:window-closed ((window window))
  (handle window (make-instance 'window:close)))

(defmethod glfw:window-resized ((window window) w h)
  (let ((bounds (alloy:bounds window)))
    (setf (alloy:bounds window) (alloy:px-extent (alloy:pxx bounds) (alloy:pxy bounds) w h))))

(defmethod glfw:window-moved ((window window) x y)
  (let ((bounds (alloy:bounds window)))
    (setf (alloy:bounds window) (alloy:px-extent x y (alloy:pxw bounds) (alloy:pxh bounds)))))

(defmethod glfw:file-dropped ((window window) files)
  (handle window (make-instance 'alloy:drop-event :paths files)))
