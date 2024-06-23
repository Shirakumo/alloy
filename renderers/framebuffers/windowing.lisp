(in-package #:org.shirakumo.alloy.renderers.framebuffers)

(defclass monitor (window:monitor)
  ((native :accessor native)))

(defmethod alloy:bounds ((monitor monitor)))

(defclass screen (window:screen)
  ())

(defmethod list-monitors ((screen screen)))

(defmethod list-windows ((screen screen)))

(defmethod alloy:bounds ((screen screen)))

(defclass window (fb:event-handler)
  ((native :accessor native)))

(defmethod make-window ((screen screen) &key title icon bounds min-size max-size

                                             state decorated-p always-on-top-p
                                             background-color
                        &allow-other-keys))

;;;; Window control
(defmethod close ((window window)))

(defmethod notify ((window window)))

(defmethod cursor ((window window)))

(defmethod move-to-front ((window window)))

(defmethod move-to-back ((window window)))

(defmethod alloy:bounds ((window window)))

(defmethod (setf alloy:bounds) (bounds (window window)))

(defmethod background-color ((window window)))

(defmethod (setf background-color) (color (window window)))

(defmethod max-size ((window window)))

(defmethod (setf max-size) (size (window window)))

(defmethod min-size ((window window)))

(defmethod (setf min-size) (size (window window)))

(defmethod decorated-p ((window window)))

(defmethod (setf decorated-p) (decorated (window window)))

(defmethod title ((window window)))

(defmethod (setf title) (title (window window)))

(defmethod icon ((window/cursor window)))

(defmethod (setf icon) (icon (window window)))

(defmethod always-on-top-p ((window window)))

(defmethod (setf always-on-top-p) (top (window window)))

(defmethod state ((window window)))

(defmethod (setf state) (state (window window)))

(defmethod fullscreen ((window window) monitor))

;;;; Event translations
(defmethod fb:window-moved ((window window) xpos ypos))

(defmethod fb:window-resized ((window window) width height))

(defmethod fb:window-refreshed ((window window)))

(defmethod fb:window-focused ((window window) focused-p))

(defmethod fb:window-iconified ((window window) iconified-p))

(defmethod fb:window-maximized ((window window) maximized-p))

(defmethod fb:window-closed ((window window)))

(defmethod fb:mouse-button-changed ((window window) button action modifiers))

(defmethod fb:mouse-moved ((window window) xpos ypos))

(defmethod fb:mouse-entered ((window window) entered-p))

(defmethod fb:mouse-scrolled ((window window) xoffset yoffset))

(defmethod fb:key-changed ((window window) key scan-code action modifiers))

(defmethod fb:string-entered ((window window) string))

(defmethod fb:file-dropped ((window window) paths))

(defmethod fb:content-scale-changed (window xscale yscale))

(defmethod fb:display-connected ((window window) display connected-p))
