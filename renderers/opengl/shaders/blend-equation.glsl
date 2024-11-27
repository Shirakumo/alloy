#extension GL_KHR_blend_equation_advanced : enable
#ifdef GL_KHR_blend_equation_advanced
  // NOTE: Even though we don't use any of the gl_Sample* variables
  //       The AMD driver on Windows complains about it being used
  //       without this extension, so... we enable it explicitly.
  //       Gotta love AMD!!!!!!!!
  #extension GL_ARB_sample_shading : enable
  layout(blend_support_all_equations) out;
#endif
