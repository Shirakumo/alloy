#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.renderers.simple
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors))
  (:import-from #:org.shirakumo.alloy #:arg! #:translate)
  ;; defaults.lisp
  (:export
   #:font
   #:family
   #:slant
   #:spacing
   #:weight
   #:stretch
   #:image
   #:size
   #:data
   #:channels
   #:gradient
   #:start
   #:stop
   #:stops
   #:linear-gradient
   #:radial-gradient
   #:angle-gradient
   #:diamond-gradient
   #:image-pattern
   #:scaling
   #:offset
   #:mode
   #:shape
   #:patterned-shape
   #:pattern
   #:filled-shape
   #:outlined-shape
   #:line-width
   #:rectangle
   #:bounds
   #:filled-rectangle
   #:outlined-rectangle
   #:ellipse
   #:filled-ellipse
   #:outlined-ellipse
   #:polygon
   #:points
   #:line-strip
   #:curve
   #:text
   #:font
   #:size
   #:bounds
   #:valign
   #:halign
   #:direction
   #:wrap
   #:markup
   #:icon
   #:shift
   #:image
   #:sizing
   #:cursor
   #:text-object
   #:start
   #:selection
   #:end
   #:resolve-alignment
   #:resolve-scale)
  ;; protocol.lisp
  (:export
   #:call-with-pushed-transforms
   #:clip
   #:translate
   #:scale
   #:rotate
   #:z-index
   #:clear
   #:composite-mode
   #:line-strip
   #:curve
   #:rectangle
   #:ellipse
   #:polygon
   #:icon
   #:text
   #:cursor
   #:selection
   #:image-pattern
   #:request-font
   #:request-image
   #:request-gradient
   #:renderer
   #:with-pushed-transforms)
  ;; transforms.lisp
  (:export
   #:matrix
   #:matrix-identity
   #:mat*
   #:transform
   #:clip-mask
   #:transform-matrix
   #:identity-matrix
   #:add-matrix
   #:transformed-renderer
   #:translate-by
   #:scale-by
   #:make-default-transform))
