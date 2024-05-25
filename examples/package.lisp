(defpackage #:org.shirakumo.alloy.examples
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:glfw #:org.shirakumo.alloy.renderers.glfw)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations)
   (#:windowing #:org.shirakumo.alloy.windowing)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:animation #:org.shirakumo.alloy.animation)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors))
  (:export
   #:simple-window
   #:fonts
   #:grid-bag-layout
   #:sizing))

(in-package #:org.shirakumo.alloy.examples)
