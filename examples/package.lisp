(defpackage #:org.shirakumo.alloy.examples
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   #+alloy-glfw (#:glfw #:org.shirakumo.alloy.renderers.glfw)
   #+alloy-framebuffers (#:framebuffers #:org.shirakumo.alloy.renderers.framebuffers)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations)
   (#:windowing #:org.shirakumo.alloy.windowing)
   (#:animation #:org.shirakumo.alloy.animation)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors))
  (:export
   #:simple-window
   #:fonts
   #:grid-bag-layout
   #:sizing))

(in-package #:org.shirakumo.alloy.examples)
