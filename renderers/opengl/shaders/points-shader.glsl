//VERT
layout (location=0) in vec2 pos;
layout (location=1) in vec2 vuv;
uniform mat3 transform;
out vec2 uv;

void main(){
  uv = vuv-0.5;
  gl_Position = vec4(transform*vec3(pos, 1.0), 1.0);
}

//FRAG
out vec4 out_color;
uniform vec4 color;
uniform float feather = 0.0;
in vec2 uv;

void main(){
  float sdf = length(uv)-0.42;
  float dsdf = fwidth(sdf)*(1.0+feather);
  sdf = smoothstep(dsdf, -dsdf, sdf);
  out_color = color*sdf;
}
