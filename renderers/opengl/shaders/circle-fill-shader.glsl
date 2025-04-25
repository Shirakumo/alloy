//VERT
layout (location=0) in vec2 pos;
uniform mat4x3 transform;
uniform float start_angle;
uniform float end_angle;
uniform vec2 size;
uniform float feather = 0.0;
out vec2 uv;

#define PI_2 1.5707963267948966

void main(){
  uv = pos+vec2(feather)/size-0.5;
  gl_Position = vec4(transform*vec4(pos, 0.0, 1.0), 1.0);
}

//FRAG
out vec4 out_color;
uniform vec4 color;
uniform float feather = 0.0;
in vec2 uv;

void main(){
  float sdf = length(uv)-0.5;
  float dsdf = fwidth(sdf)*0.5*(feather+1);
  sdf = smoothstep(dsdf, -dsdf, sdf);
  out_color = color*sdf;
  if(out_color.w <= 0.0) discard;
}
