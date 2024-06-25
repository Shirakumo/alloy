(in-package #:org.shirakumo.alloy.renderers.framebuffers)

(defclass monitor (window:monitor)
  ((native :initarg :native :accessor native)))

(defmethod alloy:bounds ((monitor monitor))
  (destructuring-bind (w h) (fb:size (native monitor))
    (destructuring-bind (x y) (fb:location (native monitor))
      (alloy:px-extent x y w h))))

(defclass screen (window:screen)
  ((monitors :initform () :accessor monitors :reader window:list-monitors)
   (windows :initform () :accessor windows :reader window:list-windows)))

(defmethod initialize-instance :after ((screen screen) &key backend)
  (fb:init backend)
  (dolist (display (fb:list-displays))
    (push (make-instance 'monitor :native display) (monitors screen))))

(defmethod alloy:bounds ((screen screen)))

(defmethod alloy:cursor ((screen screen))
  :default)

(defmethod (setf alloy:cursor) (value (screen screen))
  value)

(defmethod make-window ((screen screen) &rest args &key &allow-other-keys)
  (apply #'make-instance 'window :screen screen args))

(defmethod process-events ((screen screen) &key timeout)
  (fb:process-events (windows screen) :timeout timeout)
  (setf (windows screen) (remove-if #'fb:close-requested-p (windows screen)))
  screen)

(defclass cursor (window:cursor)
  ((native :initarg :native :accessor native)
   (icon :initform :default :accessor icon :reader window:icon)))

(defmethod (setf window:icon) (icon (cursor cursor))
  (setf (fb:cursor-icon (native cursor))
        (etypecase icon
          (simple:image (fb:make-icon (round (alloy:pxw (simple:size icon)))
                                      (round (alloy:pxh (simple:size icon)))
                                      (simple:data icon)))
          (symbol icon)))
  (setf (icon cursor) icon))

(defclass window (alloy:ui fb:event-handler
                  org.shirakumo.alloy.renderers.simple.presentations::default-look-and-feel)
  ((native :accessor native)
   (screen :initarg :screen :reader window:screen)
   (background-color :initform colors:white :accessor window:background-color)
   (icon :initform NIL :accessor icon :reader window:icon)
   (cursor :reader window:cursor)))

(defmethod initialize-instance :after ((window window) &key)
  (setf (slot-value window 'cursor) (make-instance 'cursor :window native)))

;;;; Window control
(defmethod window:close ((window window))
  (fb:close (native window)))

(defmethod window:notify ((window window))
  (fb:request-attention (native window)))

(defmethod window:move-to-front ((window window)))

(defmethod window:move-to-back ((window window)))

(defmethod alloy:bounds ((window window))
  (destructuring-bind (w h) (fb:size (native window))
    (destructuring-bind (x y) (fb:location (native window))
      (alloy:px-extent x y w h))))

(defmethod (setf alloy:bounds) (bounds (window window))
  (setf (fb:size (native window)) (list (alloy:pxw bounds) (alloy:pxh bounds)))
  (setf (fb:location (native window)) (list (alloy:pxx bounds) (alloy:pxy bounds)))
  bounds)

(defmethod window:max-size ((window window))
  (destructuring-bind (w h) (fb:maximum-size (native window))
    (alloy:px-size (or w 0) (or h 0))))

(defmethod (setf window:max-size) (size (window window))
  (if size
      (setf (fb:maximum-size (native window)) (list (alloy:pxw size) (alloy:pxh size)))
      (setf (fb:maximum-size (native window)) NIL)))

(defmethod window:min-size ((window window))
  (destructuring-bind (w h) (fb:minimum-size (native window))
    (alloy:px-size (or w 0) (or h 0))))

(defmethod (setf window:min-size) (size (window window))
  (if size
      (setf (fb:minimum-size (native window)) (list (alloy:pxw size) (alloy:pxh size)))
      (setf (fb:minimum-size (native window)) NIL)))

(defmethod window:decorated-p ((window window))
  (not (fb:borderless-p (native window))))

(defmethod (setf window:decorated-p) (decorated (window window))
  (setf (fb:borderless-p (native window)) (not decorated))
  decorated)

(defmethod window:title ((window window))
  (fb:title (native window)))

(defmethod (setf window:title) (title (window window))
  (setf (fb:title (native window)) title))

(defmethod (setf window:icon) ((icon null) (window window))
  (setf (fb:icon (native window)) icon)
  (setf (icon window) icon))

(defmethod (setf window:icon) ((icon simple:image) (window window))
  (setf (fb:icon (native window)) (fb:make-icon (round (alloy:pxw (simple:size icon)))
                                                (round (alloy:pxh (simple:size icon)))
                                                (simple:data icon)))
  (setf (icon window) icon))

(defmethod window:always-on-top-p ((window window))
  (fb:always-on-top-p (native window)))

(defmethod (setf window:always-on-top-p) (top (window window))
  (setf (fb:always-on-top-p (native window)) top))

(defmethod window:state ((window window))
  (cond ((fb:iconified-p (native window)) :minimized)
        ((not (fb:visible-p (native window))) :hidden)
        ((fb:fullscreen-p (native window)) :fullscreen)
        ((fb:maximized-p (native window)) :maximized)
        (T :normal)))

(defmethod (setf window:state) (state (window window))
  (ecase state
    (:minimized (setf (fb:iconified-p (native window)) T))
    (:maximized (setf (fb:maximized-p (native window)) T))
    (:fullscreen (setf (fb:fullscreen-p (native window)) T))
    (:hidden (setf (fb:visible-p (native window)) NIL))
    (:normal
     (setf (fb:maximized-p (native window)) NIL)
     (setf (fb:iconified-p (native window)) NIL)
     (setf (fb:fullscreen-p (native window)) NIL)
     (setf (fb:visible-p (native window)) T)))
  state)

(defmethod window:fullscreen ((window window) monitor)
  (setf (fb:fullscreen-p (native window)) monitor))

;;;; Event translations
(defmethod fb:window-moved ((window window) xpos ypos))

(defmethod fb:window-resized ((window window) width height)
  (alloy:notice-size (alloy:px-size width height) window))

(defmethod fb:window-refreshed ((window window))
  (alloy:render (window:screen window) window))

(defmethod fb:window-focused ((window window) focused-p))

(defmethod fb:window-iconified ((window window) iconified-p)
  (alloy:handle (make-instance 'window:state :new-state (if iconified-p :iconified :normal)) window))

(defmethod fb:window-maximized ((window window) maximized-p)
  (alloy:handle (make-instance 'window:state :new-state (if iconified-p :maximized :normal)) window))

(defmethod fb:window-closed ((window window))
  (alloy:handle (make-instance 'window:close) window))

(defmethod fb:mouse-button-changed ((window window) button action modifiers)
  (let ((location (pointer-location window)))
    (case action
      (:press (alloy:handle (make-instance 'alloy:pointer-down :kind button :location location) window))
      (:release (alloy:handle (make-instance 'alloy:pointer-up :kind button :location location) window)))))

(defmethod fb:mouse-moved ((window window) xpos ypos)
  (let ((location (pointer-location window)))
    (alloy:handle (make-instance 'alloy:pointer-move :old-location location :location new) window)
    (setf (pointer-location window) new)))

(defmethod fb:mouse-entered ((window window) entered-p)
  (alloy:handle (make-instance (if entered-p 'window:pointer-enter 'window:pointer-leave)) window))

(defmethod fb:mouse-scrolled ((window window) xoffset yoffset)
  (alloy:handle (make-instance 'alloy:scroll :dx xoffset :dy yoffset :location (pointer-location window)) window))

(defmethod fb:key-changed ((window window) key scan-code action modifiers)
  (alloy:handle (make-instance 'alloy:key-down :modifiers modifiers :key key :code scan-code) window))

(defmethod fb:string-entered ((window window) string)
  (alloy:handle (make-instance 'alloy:text-event :text string) window))

(defmethod fb:file-dropped ((window window) paths)
  (alloy:handle (make-instance 'alloy:drop-event :paths paths) window))

(defmethod fb:content-scale-changed (window xscale yscale)
  (alloy:handle (make-instance 'alloy:scale-changed) window))

(defmethod fb:display-connected ((window window) display connected-p)
  (if connected-p
      (push (make-instance 'monitor :native display) (displays (window:screen window)))
      (setf (displays (window:screen window)) (remove display (displays (window:screen window)) :key #'native))))
