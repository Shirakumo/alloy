//VERT
layout (location=0) in vec2 pos;
uniform mat4 transform;
out vec2 uv;

void main(){
  gl_Position = transform*vec4(pos, 0.0, 1.0);
  uv = pos;
}

//FRAG
out vec4 out_color;
uniform sampler2D image;
uniform vec2 uv_offset = vec2(0,0);
uniform vec2 uv_scale = vec2(1,1);
in vec2 uv;

void main(){
  vec4 color = texture(image, (uv/uv_scale)+uv_offset);
  out_color = vec4(color.rgb*color.a, color.a);
}
