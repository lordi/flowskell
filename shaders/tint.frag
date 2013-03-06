uniform sampler2D colorMap;
uniform vec3 tintColor;
uniform float alpha;

void main()
{
  vec3 pixelColor = vec3(texture2D( colorMap, gl_TexCoord[0].st));
  gl_FragColor = vec4(tintColor * pixelColor, alpha);
}
