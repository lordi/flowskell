#version 110

uniform sampler2D colorMap;
uniform float amt;
void main (void)
{
	vec3 pixelColor = vec3(texture2D( colorMap, gl_TexCoord[0].st));
	gl_FragColor = vec4(pixelColor, amt);
}
