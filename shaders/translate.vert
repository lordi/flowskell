uniform float x;
uniform float y;

void main()
{
    gl_TexCoord[0] = gl_MultiTexCoord0;
    gl_TexCoord[0][0] += x;
    gl_TexCoord[0][1] += y;
    gl_Position = ftransform();
}
