#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.windowing
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy))
  (:shadow #:close)
  (:export
   #:*default-window-bounds*
   #:*default-window-title*
   #:*default-window-icon*
   #:icon
   #:make-icon
   #:cursor
   #:monitor
   #:screen
   #:list-windows
   #:list-monitors
   #:size
   #:window
   #:layout-element
   #:focus-element
   #:make-window
   #:close
   #:notify
   #:cursor
   #:move-to-front
   #:move-to-back
   #:max-size
   #:min-size
   #:decorated-p
   #:title
   #:icon
   #:always-on-top-p
   #:state
   #:fullscreen
   #:window-event
   #:pointer-enter
   #:pointer-leave
   #:close
   #:state
   #:new-state))
