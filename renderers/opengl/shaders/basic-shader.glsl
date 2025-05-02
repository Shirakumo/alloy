//VERT
layout (location=0) in vec2 pos;
uniform mat4 transform;

void main(){
  gl_Position = transform*vec4(pos, 0.0, 1.0);
}

//FRAG
out vec4 out_color;
uniform vec4 color;

void main(){
  out_color = vec4(color.rgb*color.a, color.a);
}
