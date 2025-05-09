(defpackage #:org.shirakumo.alloy.renderers.simple
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors))
  (:import-from #:org.shirakumo.alloy #:arg! #:translate)
  ;; canvas.lisp
  (:export
   #:canvas-style
   #:pattern
   #:line-width
   #:line-style
   #:join-style
   #:cap-style
   #:family
   #:size
   #:canvas
   #:draw-shape
   #:draw-rectangle
   #:draw-ellipse
   #:draw-curve
   #:fill-rectangle
   #:fill-ellipse
   #:draw-text
   #:draw-image
   #:start-line
   #:start-polygon
   #:complete-shape
   #:move-to
   #:push-matrix
   #:pop-matrix
   #:style)
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
   #:feather-radius
   #:outlined-shape
   #:line-width
   #:line-style
   #:join-style
   #:cap-style
   #:rectangle
   #:bounds
   #:corner-radii
   #:corner-radius
   #:filled-rectangle
   #:outlined-rectangle
   #:ellipse
   #:start-angle
   #:end-angle
   #:filled-ellipse
   #:outlined-ellipse
   #:polygon
   #:points
   #:line-strip
   #:curve
   #:sort-markup
   #:flatten-markup
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
   #:with-matrix
   #:matrix
   #:copy-matrix
   #:matrix-identity
   #:with-matrix
   #:mat*p
   #:mat*v
   #:mat*
   #:transform
   #:clip-mask
   #:transform-matrix
   #:identity-matrix
   #:add-matrix
   #:transformed-renderer
   #:translate-by
   #:scale-by
   #:orthographic-matrix
   #:activate-perspective
   #:perspective-matrix
   #:make-default-transform))
