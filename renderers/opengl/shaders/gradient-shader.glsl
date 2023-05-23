//VERT
layout (location=0) in vec2 pos;
layout (location=1) in vec4 vertex_color;

out vec4 color;
uniform mat3 transform;

void main(){
  gl_Position = vec4(transform*vec3(pos, 1.0), 1.0);
  color = vertex_color;
}

//FRAG
out vec4 out_color;
in vec4 color;

void main(){
  out_color = vec4(color.rgb*color.a, color.a);
}
