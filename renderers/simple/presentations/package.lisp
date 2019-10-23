#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.renderers.simple.presentations
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors))
  (:import-from #:org.shirakumo.alloy #:arg!)
  ;; default.lisp
  (:export
   #:default-look-and-feel)
  ;; protocol.lisp
  (:export
   #:renderer
   #:name
   #:composite-mode
   #:z-index
   #:offset
   #:scale
   #:rotation
   #:pivot
   #:hidden-p
   #:shapes
   #:update-overrides
   #:override-shapes
   #:realize-renderable
   #:update-shape
   #:clear-shapes
   #:find-shape
   #:define-realization
   #:define-update))
