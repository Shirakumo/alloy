//VERT
layout (location=0) in vec2 pos;
layout (location=1) in vec2 weight;
uniform mat4x3 transform;
uniform float corner_radius[5];
uniform vec2 size;
uniform float feather = 0.0;
out vec2 uv;

void main(){
  vec2 position = pos*size;
  ivec2 corner_idx = abs(ivec2(weight));
  vec2 offset_dir = 1.0-pos*2.0;
  vec2 offset = vec2(corner_radius[corner_idx.x], corner_radius[corner_idx.y]);
  vec2 rpos = position+offset*offset_dir;

  gl_Position = vec4(transform*vec4(rpos, 0.0, 1.0), 1.0);
  uv = 1+vec2(feather)/size-sign(weight);
}

//FRAG
out vec4 out_color;
uniform vec4 color;
uniform float feather = 0.0;
in vec2 uv;

void main(){
  float sdf = max(uv.x,uv.y)-1.0;
  float dsdf = fwidth(sdf)*0.5*(feather+1);
  sdf = smoothstep(dsdf, -dsdf, sdf);
  out_color = color*sdf;
}
