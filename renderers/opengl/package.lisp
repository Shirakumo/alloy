(defpackage #:org.shirakumo.alloy.renderers.opengl
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors))
  ;; protocol.lisp
  (:export
   #:view-size
   #:bind
   #:gl-name
   #:make-shader
   #:uniform
   #:make-vertex-buffer
   #:update-vertex-buffer
   #:make-vertex-array
   #:draw-vertex-array
   #:make-texture
   #:make-framebuffer
   #:blit-framebuffer)
  ;; renderer.lisp
  (:export
   #:renderer
   #:resource))
