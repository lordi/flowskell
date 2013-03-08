#define PI 3.141592653
void main(void) 
{
    vec2 v = gl_TexCoord[0].xy * PI;
    float intens = abs(sin(abs(v.y))*sin(abs(v.x))*1.2);
    gl_FragColor = gl_Color;
    gl_FragColor = vec4(intens * vec3(gl_Color), 1.0);
}
