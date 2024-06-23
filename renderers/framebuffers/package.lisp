(defpackage #:org.shirakumo.alloy.renderers.framebuffers
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl)
   (#:window #:org.shirakumo.alloy.windowing)
   (#:animation #:org.shirakumo.alloy.animation)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:fb #:org.shirakumo.framebuffers))
  (:export
   #:renderer
   #:cursor
   #:monitor
   #:screen
   #:with-screen
   #:window))
