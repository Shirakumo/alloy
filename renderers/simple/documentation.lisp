#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(docs:define-docs
  (function line-style
    "The style of the line being drawn.

May be one of:
  NIL      --- The backend's default is used.
  :SOLID   --- The line is solid throughout.
  :DASHED  --- The line is composed out of segments with gaps.
  :DOTTED  --- The line is composed out of square or round dots.

Backends may offer more styles. If a backend does not support a style,
it must treat it as NIL.")

  (function join-style
    "The style of the joins between the outwards-facing sides of line segments.

May be one of:
  NIL    --- The backend's default is used.
  :NONE  --- The line segments are bare and not joined.
  :BEVEL --- The sides are linearly connected.
  :ROUND --- The sides are joined via a circle.
  :MITER --- The sides are extended until they meet.

Backends may offer more styles. If a backend does not support a style,
it must treat it as NIL.")

  (function cap-style
    "The style of the line end caps.

May be one of:
  NIL     --- The backend's default is used.
  :ROUND  --- The line ends are rounded.
  :SQUARE --- The line ends are squares.
  :BUTT   --- The line ends are truncated.
  :SPIKE  --- The line ends in triangles.

Backends may offer more styles. If a backend does not support a style,
it must treat it as NIL."))
