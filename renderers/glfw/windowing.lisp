#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.glfw)

(defclass icon (window:icon)
  ())

(defmethod window:make-icon (renderer size pixel-data))

(defclass cursor (window:cursor)
  ())

(defmethod window:locked-p (cursor))
(defmethod (setf window:locked-p) (cursor))

(defclass ui (window:screen)
  ())

(defgeneric window:list-monitors (screen))
(defgeneric window:size (screen))

(defclass window (window:window renderer)
  ())

(defmethod make-window (screen &key title icon bounds min-size max-size
                                    visible-p minimized-p maximized-p decorated-p always-on-top-p
                                    &allow-other-keys))
(defmethod window:close (window))
(defmethod window:notify (window))
(defmethod window:cursor (window))
(defmethod window:move-to-front (window))
(defmethod window:move-to-back (window))
(defmethod window:max-size (window))
(defmethod (setf window:max-size) (size window))
(defmethod window:min-size (window))
(defmethod (setf window:min-size) (size window))
(defmethod window:visible-p (window/cursor))
(defmethod (setf window:visible-p) (visible window/cursor))
(defmethod window:minimized-p (window))
(defmethod (setf window:minimized-p) (window))
(defmethod window:maximized-p (window))
(defmethod (setf window:maximized-p) (window))
(defmethod window:decorated-p (window))
(defmethod (setf window:decorated-p) (window))
(defmethod window:title (window))
(defmethod (setf window:title) (title window))
(defmethod window:icon (window/cursor))
(defmethod (setf window:icon) (icon window/cursor))
(defmethod window:always-on-top-p (top window))
(defmethod (setf window:always-on-top-p) (top window))
(defmethod window:fullscreen (window monitor))
(defmethod window:restore (window))
