//VERT
layout (location=0) in vec2 pos;
layout (location=1) in vec2 weight;
uniform mat3 transform;
uniform float corner_radius[5];
uniform vec2 size;

void main(){
  vec2 position = pos*size;
  ivec2 corner_idx = abs(ivec2(weight));
  vec2 offset_dir = 1.0-pos*2.0;
  vec2 offset = vec2(corner_radius[corner_idx.x], corner_radius[corner_idx.y]);

  gl_Position = vec4(transform*vec3(position+offset*offset_dir, 1.0), 1.0);
}

//FRAG
#extension GL_KHR_blend_equation_advanced : enable
#ifdef GL_KHR_blend_equation_advanced
layout(blend_support_all_equations) out;
#endif
out vec4 out_color;
uniform vec4 color;

void main(){
  out_color = vec4(color.rgb*color.a, color.a);
}
