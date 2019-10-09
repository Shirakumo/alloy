#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.windowing)

(defclass icon ()
  ())

(defclass cursor ()
  ())

(defgeneric locked-p (cursor))
(defgeneric (setf locked-p) (cursor))

(defclass monitor ()
  ())

(defgeneric resolution (monitor))
(defgeneric (setf resolution) (size monitor))

(defclass frame ()
  ())

(defgeneric cursor (frame))
(defgeneric (setf cursor) (cursor frame))
(defgeneric size (frame))

(defclass screen (frame)
  ())

(defgeneric screen (ui))
(defgeneric list-monitors (screen))

(defclass window (frame)
  ())

(defclass popup (window)
  ())

(defclass utility (window)
  ())

(defgeneric make-window (screen &key title icon bounds min-size max-size
                                     visible-p minimized-p maximized-p decorated-p always-on-top-p))
(defgeneric close (window))
(defgeneric notify (window))
(defgeneric move-to-front (window))
(defgeneric move-to-back (window))
(defgeneric location (window/cursor))
(defgeneric (setf location) (window/cursor))
(defgeneric (setf size) (size window))
(defgeneric bounds (window))
(defgeneric (setf bounds) (extent window))
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
(defgeneric focused-p (window))
(defgeneric (setf focused-p) (focus window))
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
