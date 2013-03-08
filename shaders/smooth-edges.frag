#define PI 3.141592653
void main(void) 
{
    vec2 v = gl_TexCoord[0].xy * PI;
    float intens = abs(sin(abs(v.y))*sin(abs(v.x))*2.0);
    gl_FragColor = intens * gl_Color;
}
