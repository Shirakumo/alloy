#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.opengl)

(docs:define-docs
  (function view-size
    "Return an ALLOY:SIZE describing the current view size of the context.

See ALLOY:SIZE")
  
  (function bind
    "Activate or bind the given resource.

For shaders: the shader program is activated.
For textures: the texture is bound as Texture0.
For framebuffers: the framebuffer is set as the draw framebuffer.

See MAKE-SHADER
See MAKE-TEXTURE
See MAKE-FRAMEBUFFER")
  
  (function gl-name
    "Returns the internal OpenGL resource id for the given resource.")
  
  (function make-shader
    "Create a shader program

VERTEX-SHADER and FRAGMENT-SHADER are GLSL shader source code that
should be bound to the resulting shader program.

The returned value must have applicable methods for BIND and (SETF
UNIFORM).

See BIND
See (SETF UNIFORM)")
  
  (function (setf uniform)
    "Set the OpenGL uniform variable state in the given shader program to the specified value.

The following values must be accepted as the specified OpenGL value types:
  ALLOY:SIZE         vec2
  ALLOY:POINT        vec2
  COLORED:COLOR      vec4
  VECTOR             mat3
  SINGLE-FLOAT       float
  (SIGNED-BYTE 32)   int

The backend will access the following locations in shaders:
  transform
  color
  line_width
  gap
  view_size
  size
  corner_radius[0]
  corner_radius[1]
  corner_radius[2]
  corner_radius[3]
  corner_radius[4]
  start_angle
  end_angle
  uv_scale
  uv_offset
")
  
  (function make-vertex-buffer
    "Create a vertex buffer object with the given contents.

BUFFER-TYPE may be one of:
  VERTEX-BUFFER  --- The default vertex data buffer
  ELEMENT-BUFFER --- An element index array buffer

DATA-USAGE may be one of:
  STATIC-DRAW    --- The data is only uploaded once, on creation
  STREAM-DRAW    --- The data is updated frequently

The CONTENTS should be a vector with element type SINGLE-FLOAT for
VERTEX-BUFFER, or (UNSIGNED-BYTE 32) for ELEMENT-BUFFER types.

See UPDATE-VERTEX-BUFFER
See MAKE-VERTEX-ARRAY")
  
  (function update-vertex-buffer
    "Update the contents of the buffer object with the given contents.

The buffer must be resized to fit and filled with the given contents.
The CONTENTS should be a vector with element type SINGLE-FLOAT for
VERTEX-BUFFER, or (UNSIGNED-BYTE 32) for ELEMENT-BUFFER types.

The behaviour is undefined for calling this function with a vertex
buffer created with STATIC-DRAW DATA-USAGE.

See MAKE-VERTEX-BUFFER")
  
  (function make-vertex-array
    "Create a vertex array object with the given buffer binding points.

BINDINGS should be a list of the following format:
  BINDINGS      ::= BINDING-POINT*
  BINDING-POINT ::= (resource :SIZE size :OFFSET offset :STRIDE stride)
  resource      --- A vertex buffer
  size          --- The number of elements in the binding, 1/2/3/4
  offset        --- The offset (in bytes) to the start of the element
  stride        --- The stride (in bytes) between the elements

For instance, the following GLSL vertex shader input spec:

  layout (location = 0) in vec2 pos;
  layout (location = 1) in float time;

Corresponds to the following binding spec:
 
  ((:size 2 :offset 0 :stride 12) (:size 1 :offset 8 :stride 12))

If INDEX-BUFFER is given, it designates the element index array.

See MAKE-VERTEX-BUFFER
See DRAW-VERTEX-ARRAY")
  
  (function draw-vertex-array
    "Draw the given vertex array.

PRIMITIVE-TYPE may be one of:
  :LINES
  :TRIANGLES
  :TRIANGLE-STRIP
  :TRIANGLE-FAN

OFFSET specifies the offset in elements from the beginning of the
vertex array.

COUNT specifies the number of vertices to draw.

See MAKE-VERTEX-ARRAY")
  
  (function make-texture
    "Allocate a new texture.

The texture should be a Texture-2D, with the specified with, height,
data, channels, and filtering.

DATA should be an (UNSIGNED-BYTE 8) vector.
CHANNELS should be one of the following, specifying the associated
internal format and pixel format:

  1 :RED
  2 :RG
  3 :RGB
  4 :RGBA

The texture swizzling should be applied as follows:

  1 :RED :RED :RED :ONE
  2 :RED :RED :RED :GREEN
  3 :RED :GREEN :BLUE :ONE
  4 :RED :GREEN :BLUE :ALPHA

FILTERING specifies the min and mag filter of the texture, and can be
one of the following:

  :NEAREST
  :LINEAR

See BIND")
  
  (function make-framebuffer
    "Create a new framebuffer.

The framebuffer must have attachments for a color buffer, a depth
buffer, and a stencil buffer.

The buffers must automatically be adjusted to fit the VIEW-SIZE, and
thus be usable as a replacement backing render buffer.

See BIND
See BLIT-FRAMEBUFFER")
  
  (function blit-framebuffer
    "Blits the bound framebuffer to the backing render buffer.

After this call the framebuffer will be unbound, and the backing
render buffer should be bound as the draw framebuffer.

See MAKE-FRAMEBUFFER"))
