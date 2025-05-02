//VERT
layout (location=0) in vec2 pos;
uniform mat4 transform;
uniform float start_angle;
uniform float end_angle;
out vec2 uv;
out vec2 c;

#define PI_2 1.5707963267948966

void main(){
  uv = pos-0.5;
  float start = start_angle-PI_2;
  float aperture = abs(end_angle-start_angle)*0.5;
  if(end_angle < start_angle){
    start += -2*(aperture+PI_2);
    aperture -= 2*PI_2;
  }

  c = vec2(sin(aperture), cos(aperture));
  float cstart = cos(aperture+start);
  float sstart = sin(aperture+start);
  uv = mat2(cstart,-sstart,
            sstart, cstart)*uv;
  gl_Position = transform*vec4(pos, 0.0, 1.0);
}

//FRAG
out vec4 out_color;
uniform vec4 color;
uniform float line_width = 3.0;
uniform float gap = 0.0;
in vec2 uv;
in vec2 c;

void main(){
  vec2 p = vec2(abs(uv.x), uv.y);
  float l = length(p)-0.5;
  float m = length(p-c*clamp(dot(p,c),0.0,0.5));
  float sdf = max(max(l,m*sign(c.y*p.x-c.x*p.y)), (0.5-line_width)-length(uv));
  float dsdf = fwidth(sdf)*0.5;
  sdf = smoothstep(dsdf, -dsdf, sdf);

  float time = atan(uv.y, uv.x);
  float t = time/(line_width*0.3)*gap;
  out_color = color*sdf*clamp(1-sin(t)*4, 0.0, 1.0);
  if(out_color.w <= 0.0) discard;
}
