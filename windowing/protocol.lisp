#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.windowing)

(defclass icon ()
  ())

(defgeneric make-icon (renderer size pixel-data))

(defclass cursor ()
  ())

(defgeneric locked-p (cursor))
(defgeneric (setf locked-p) (cursor))

(defclass monitor ()
  ())

;; alloy:bounds
;; (setf alloy:bounds)

(defclass screen (alloy:ui)
  ())

(defgeneric list-monitors (screen))
(defgeneric size (screen))

(defclass window (alloy:layout-element alloy:focus-element)
  ())

(defclass popup (window) ())
(defclass utility (window) ())

(defgeneric make-window (screen &key title icon bounds min-size max-size
                                     visible-p minimized-p maximized-p decorated-p always-on-top-p
                         &allow-other-keys))
(defgeneric close (window))
(defgeneric notify (window))
(defgeneric cursor (window))
(defgeneric move-to-front (window))
(defgeneric move-to-back (window))
(defgeneric max-size (window))
(defgeneric (setf max-size) (size window))
(defgeneric min-size (window))
(defgeneric (setf min-size) (size window))
(defgeneric visible-p (window/cursor))
(defgeneric (setf visible-p) (visible window/cursor))
(defgeneric minimized-p (window))
(defgeneric (setf minimized-p) (window))
(defgeneric maximized-p (window))
(defgeneric (setf maximized-p) (window))
(defgeneric decorated-p (window))
(defgeneric (setf decorated-p) (window))
(defgeneric title (window))
(defgeneric (setf title) (title window))
(defgeneric icon (window/cursor))
(defgeneric (setf icon) (icon window/cursor))
(defgeneric always-on-top-p (top window))
(defgeneric (setf always-on-top-p) (top window))
(defgeneric fullscreen (window monitor))
(defgeneric restore (window))
