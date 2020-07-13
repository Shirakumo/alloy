#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.examples
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:glfw #:org.shirakumo.alloy.renderers.glfw)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:windowing #:org.shirakumo.alloy.windowing)
   (#:colors #:org.shirakumo.alloy.colored.colors))
  (:export
   #:simple-window))

(in-package #:org.shirakumo.alloy.examples)
