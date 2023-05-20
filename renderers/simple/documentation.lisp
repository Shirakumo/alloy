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
  :NONE   --- The line ends are truncated.
  :ROUND  --- The line ends are rounded.
  :SQUARE --- The line ends are squares.
  :SPIKE  --- The line ends in triangles.

Backends may offer more styles. If a backend does not support a style,
it must treat it as NIL.")

  (function image-pattern
    "Turn an image into a pattern.

IMAGE must be an object returned by REQUEST-IMAGE of the same
renderer.

SCALING should be an ALLOY:SIZE that is used to scale the image's
dimensions as a pattern.

OFFSET should be an ALLOY:POINT that is used to offset the image
pattern relative to the global coordinate system.

MODE defines how the pattern is applied. Should be one of:
  :REPEAT  --- Repeats the image across the bounding box.
  :STRETCH --- Scales the image to fill the bounding box.
  :FILL    --- Scales the image uniformly to fill the bounding box.
  :CLAMP   --- Clamps the pattern's colours at the borders

MODE defaults to :REPEAT.")

  (function request-font
    "Finds a font close to the requested specification.

This may consult fonts on the current operating system or some other
database. The exact behaviour depends on the backend.")

  (function request-image
    "Load an image from file or a pixel buffer.

DATA may be a pathname or an UNSIGNED-BYTE 8 vector. Whether a
specific file type is supported depends on the backend, however. When
passing raw pixel data, you must also pass SIZE and CHANNELS, to
designate the dimensions and channel count of the pixel data.")

  (function request-gradient
    "Load a gradient pattern.

TYPE may be one of:
  :LINEAR  --- A linear gradient along START-STOP
  :RADIAL  --- A radial gradient centred at START with STOP as radius
  :ANGLE   --- An angle gradient centred at START with STOP as radius
  :DIAMOND --- A diamond gradient centred at START with STOP as radius

START and STOP should be ALLOY:POINTs that designate the bounding
locations of the gradient pattern. STOPS should be a vector of conses,
where each cons has two elements:
  STOP  --- A single-float in [0,1] designating the stop at which to
            apply the associated colour.
  COLOR --- A COLORED:COLOR for the stop."))
