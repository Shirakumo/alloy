//VERT
layout (location=0) in vec2 pos;
uniform mat3 transform;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1.0), 1.0);
}

//FRAG
#extension GL_KHR_blend_equation_advanced : enable
uniform vec4 color;
#ifdef GL_KHR_blend_equation_advanced
layout(blend_support_all_equations) out vec4 out_color;
#else
out vec4 out_color;
#endif

void main(){
  out_color = vec4(color.rgb*color.a, color.a);
}
