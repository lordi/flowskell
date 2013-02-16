void main()
{
  gl_Position = gl_ModelViewProjectionMatrix * (vec4(1.0, 1.0, 1.0, 1.0) * gl_Vertex);
}
 
