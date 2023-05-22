//VERT
layout (location=0) in vec2 pos;
uniform mat3 transform;
out vec2 uv;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1.0), 1.0);
  uv = pos;
}

//FRAG
#extension GL_KHR_blend_equation_advanced : enable
#ifdef GL_KHR_blend_equation_advanced
layout(blend_support_all_equations) out;
#endif
out vec4 out_color;
uniform sampler2D image;
uniform vec2 uv_offset = vec2(0,0);
uniform vec2 uv_scale = vec2(1,1);
in vec2 uv;

void main(){
  vec4 color = texture(image, (uv/uv_scale)+uv_offset);
  out_color = vec4(color.rgb*color.a, color.a);
}
