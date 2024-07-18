(defpackage #:org.shirakumo.alloy.renderers.framebuffers
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:window #:org.shirakumo.alloy.windowing)
   (#:animation #:org.shirakumo.alloy.animation)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:fb #:org.shirakumo.framebuffers)
   (#:raster #:org.shirakumo.raster))
  (:export
   #:renderer
   #:cursor
   #:monitor
   #:screen
   #:window))

(push :alloy-framebuffers *features*)
