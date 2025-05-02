//VERT
layout(location = 0) in vec2 pos;
layout(location = 1) in vec2 normal;
layout(location = 2) in float time;

out vec2 line_normal;
out float t;
uniform float line_width = 3.0;
uniform float gap = 0.0;
uniform mat4 transform;
uniform vec2 view_size;

void main(){
  gl_Position = transform*vec4(pos, 0.0, 1.0);
  line_normal = normal;
  t = time/(line_width*0.3)*gap;
}

//FRAG
out vec4 out_color;
in vec2 line_normal;
in float t;
uniform float feather = 0.3;
uniform vec4 color;

void main(){
  out_color = color * ((1-length(line_normal))/feather) * clamp(1-sin(t)*4, 0.0, 1.0);
  if(out_color.w <= 0.0) discard;
}
