//VERT
layout (location=0) in vec2 pos;
layout (location=1) in vec2 in_uv;
layout (location=2) in vec4 in_vert_color;
layout (location=3) in vec3 in_offset;
layout (location=4) in vec4 in_outline;
uniform mat3 transform;
out vec2 uv;
out vec4 vert_color;
out vec4 outline;
out float bias;

void main(){
  gl_Position = vec4(transform*vec3(pos+in_offset.xy, 1.0), 1.0);
  uv = in_uv;
  vert_color = in_vert_color;
  outline = in_outline;
  bias = in_offset.z;
}

//FRAG
#extension GL_KHR_blend_equation_advanced : enable
#ifdef GL_KHR_blend_equation_advanced
layout(blend_support_all_equations) out;
#endif
out vec4 out_color;
in vec2 uv;
in vec4 vert_color;
in vec4 outline;
in float bias;
uniform sampler2D image;
uniform float pxRange = 32.0;
uniform vec4 color = vec4(0, 0, 0, 1);

float median(float r, float g, float b){
  return max(min(r, g), min(max(r, g), b));
}

vec2 safeNormalize(in vec2 v){
  float len = length(v);
  len = (len > 0.0)? 1.0 / len : 0.0;
  return v * len;
}

void main(){
  vec3 outline_color = outline.xyz;
  float outline_thickness = outline.w;

  vec4 sample = texture(image, uv);
  float sigDist = median( sample.r, sample.g, sample.b ) - 0.5;
  
  ivec2 sz = textureSize( image, 0 );
  float dx = dFdx( uv.x ) * sz.x;
  float dy = dFdy( uv.y ) * sz.y;
  float toPixels = pxRange * inversesqrt( dx * dx + dy * dy );
  float outline_opacity = clamp( sigDist * toPixels + 0.5 + bias + outline_thickness, 0.0, 1.0 );
  float opacity = clamp( sigDist * toPixels + 0.5 + bias, 0.0, 1.0 );

  vec4 frag_col = mix(color, vert_color, vert_color.a);
  out_color = frag_col;
  if(0.0 < outline_opacity*outline_thickness){
    out_color.a *= outline_opacity;
    out_color.rgb = mix(out_color.rgb, outline_color, 1-opacity);
  } else {
    out_color.a *= opacity;
  }
  out_color.rgb *= out_color.a;
}
