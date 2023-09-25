(defpackage #:org.shirakumo.alloy.renderers.glfw
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl)
   (#:window #:org.shirakumo.alloy.windowing)
   (#:animation #:org.shirakumo.alloy.animation)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:glfw #:org.shirakumo.fraf.glfw)
   (#:%glfw #:org.shirakumo.fraf.glfw.cffi))
  (:export
   #:renderer
   #:cursor
   #:monitor
   #:screen
   #:with-screen
   #:window))
