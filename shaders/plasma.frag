uniform sampler2D colorMap;
uniform float time;

#define PI 3.14159
#define TWO_PI (PI*2.0)
#define N 5.0

void main(void) 
{
	vec2 v = gl_TexCoord[0].xy*1.5;

	float col = 0.0;

	for(float i = 0.0; i < N; i++) 
	{
	  	float a = i * (TWO_PI/N);
		col += cos(TWO_PI*(v.x * cos(a) + v.y * sin(a)+ sin(time*0.0004)*100.0 ));
	}
	
	 col /= 3.0;

	gl_FragColor = vec4(col*2.0, col*1.5,-col*3.0, 1.0);
}
