(in-package #:org.shirakumo.alloy.windowing)

(defvar *default-window-bounds* (alloy:px-size 100 100))
(defvar *default-window-title* "Alloy")
(defvar *default-window-icon* NIL)

(defclass icon ()
  ())

(defgeneric make-icon (renderer size pixel-data))

(defclass cursor ()
  ())

(defclass monitor ()
  ())

(defclass screen ()
  ((windows :initform (make-hash-table :test 'eq) :accessor windows)))

(defgeneric list-monitors (screen))
(defgeneric list-windows (screen))
(defgeneric size (monitor/screen))
(defgeneric location (monitor/screen))

(defclass window (alloy:ui alloy:renderable alloy:observable)
  ((screen :reader screen)))

(defgeneric make-window (screen &key title icon bounds min-size max-size
                                     state decorated-p always-on-top-p
                                     background-color))
(defgeneric close (window))
(defgeneric notify (window))
(defgeneric cursor (window))
(defgeneric move-to-front (window))
(defgeneric move-to-back (window))
;; alloy:bounds
;; (setf alloy:bounds)
(defgeneric background-color (window))
(defgeneric (setf background-color) (color window))
(defgeneric max-size (window))
(defgeneric (setf max-size) (size window))
(defgeneric min-size (window))
(defgeneric (setf min-size) (size window))
(defgeneric decorated-p (window))
(defgeneric (setf decorated-p) (decorated window))
(defgeneric title (window))
(defgeneric (setf title) (title window))
(defgeneric icon (window/cursor))
(defgeneric (setf icon) (icon window/cursor))
(defgeneric always-on-top-p (window))
(defgeneric (setf always-on-top-p) (top window))
(defgeneric state (window/cursor))
(defgeneric (setf state) (state window/cursor))
(defgeneric fullscreen (window monitor))

(defclass window-event (alloy:event)
  ())

(defclass pointer-enter (window-event alloy:pointer-event)
  ())

(defclass pointer-leave (window-event alloy:pointer-event)
  ())

(defclass close (window-event alloy:direct-event)
  ())

(defclass state (window-event alloy:direct-event)
  ((new-state :initarg :new-state :initform (alloy:arg! :new-state) :reader new-state)))

(defmethod alloy:extent-visible-p ((extent alloy:extent) (screen screen))
  T)

(defmethod alloy:enter ((window window) (screen screen) &key)
  (setf (slot-value window 'screen) screen)
  (setf (gethash window (windows screen)) window))

(defmethod alloy:leave ((window window) (screen screen))
  (remhash window (windows screen))
  window)

(defmethod alloy:deallocate :after ((window window))
  (alloy:leave window (screen window)))

(defmethod make-window :around ((screen screen) &key &allow-other-keys)
  (let ((window (call-next-method)))
    (alloy:enter window screen)))

(defmethod list-windows ((screen screen))
  (loop for window being the hash-values of (windows screen)
        collect window))
