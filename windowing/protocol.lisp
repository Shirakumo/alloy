#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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

(defclass screen (alloy:ui)
  ())

(defmethod initialize-instance :after ((screen screen) &key)
  (make-instance 'alloy:focus-list :focus-parent (alloy:focus-tree screen))
  (make-instance 'alloy:fixed-layout :layout-parent (alloy:layout-tree screen)))

(defgeneric list-monitors (screen))
(defgeneric list-windows (screen))
(defgeneric size (monitor/screen))

(defclass window (alloy:layout-element alloy:focus-element alloy:renderable)
  ((layout-element :initform NIL :accessor layout-element)
   (focus-element :initform NIL :accessor focus-element)))

(defgeneric make-window (screen &key title icon bounds min-size max-size
                                     state decorated-p always-on-top-p
                         &allow-other-keys))
(defgeneric close (window))
(defgeneric notify (window))
(defgeneric cursor (window))
(defgeneric move-to-front (window))
(defgeneric move-to-back (window))
;; alloy:bounds
;; (setf alloy:bounds)
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

(defclass pointer-enter (alloy:pointer-event window-event)
  ())

(defclass pointer-leave (alloy:pointer-event window-event)
  ())

(defclass close (window-event alloy:direct-event)
  ())

(defclass state (window-event)
  ((new-state :initarg :new-state :initform (alloy:arg! :new-state) :reader new-state)))

(defmethod alloy:handle ((event alloy:pointer-event) (window window) ctx)
  (when (layout-element window)
    (alloy:handle event (layout-element window) ctx)))

(defmethod alloy:render ((renderer alloy:renderer) (window window))
  (when (layout-element window)
    (alloy:render renderer (layout-element window))))

(defmethod alloy:maybe-render ((renderer alloy:renderer) (window window))
  (when (layout-element window)
    (alloy:maybe-render renderer (layout-element window))))

(defmethod alloy:enter ((element alloy:layout-element) (window window) &key)
  (when (layout-element window)
    (cerror "Replace the element" 'alloy:place-already-occupied
            :existing (layout-element window) :place T :element element :layout window)
    (alloy:leave (layout-element window) window))
  (setf (layout-element window) element))

(defmethod alloy:enter ((element alloy:focus-element) (window window) &key)
  (when (focus-element window)
    (cerror "Replace the element" 'alloy:place-already-occupied
            :existing (focus-element window) :place T :element element :focus window)
    (alloy:leave (focus-element window) window))
  (setf (focus-element window) element))

(defmethod alloy:leave ((element alloy:layout-element) (window window))
  (setf (layout-element window) NIL))

(defmethod alloy:leave ((element alloy:focus-element) (window window))
  (setf (focus-element window) NIL))

(defmethod (setf alloy:bounds) :after (extent (window window))
  (alloy:mark-for-render window)
  (when (layout-element window)
    (setf (alloy:bounds (layout-element window)) (alloy:extent 0 0 (alloy:w extent) (alloy:h extent)))))
