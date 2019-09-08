#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.renderers.simple
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy))
  ;; protocol.lisp
  (:export
   #:call-with-pushed-transforms
   #:clip
   #:translate
   #:scale
   #:rotate
   #:z-index
   #:call-with-pushed-styles
   #:fill-color
   #:line-width
   #:fill-mode
   #:composite-mode
   #:font
   #:font-size
   #:line
   #:rectangle
   #:ellipse
   #:polygon
   #:text
   #:image
   #:size
   #:data
   #:clear
   #:request-font
   #:request-image
   #:color
   #:color-p
   #:copy-color
   #:r
   #:g
   #:b
   #:a
   #:font
   #:family
   #:slant
   #:spacing
   #:weight
   #:stretch
   #:image
   #:renderer
   #:with-pushed-transforms
   #:with-pushed-styles)
  ;; transforms.lisp
  (:export
   #:matrix
   #:matrix-identity
   #:mat*
   #:transform
   #:clip-mask
   #:transform-matrix
   #:add-matrix
   #:transformed-renderer
   #:make-default-transform)
  ;; style.lisp
  (:export
   #:style
   #:styled-renderer
   #:make-default-style))
