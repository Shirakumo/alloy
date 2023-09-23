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
   #:location
   #:window
   #:layout-element
   #:focus-element
   #:make-window
   #:close
   #:notify
   #:cursor
   #:move-to-front
   #:move-to-back
   #:background-color
   #:max-size
   #:min-size
   #:decorated-p
   #:background-color
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
