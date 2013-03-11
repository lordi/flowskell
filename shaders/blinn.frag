varying vec3 v_N;

void main () {
float amb = 0.1;
vec3 color = vec3(gl_Color);
vec3 N = normalize(v_N);
vec3 L = normalize(gl_LightSource[0].position.xyz);
vec3 H = normalize(gl_LightSource[0].halfVector.xyz);

vec3 ambient = amb * color;
vec3 diffuse = color * (1.0 - amb) * max(dot(L, N), 0.0);
vec3 specular = vec3(1.0, 1.0, 1.0) * pow(max(dot(H, N), 0.0), 16.0);

gl_FragColor = vec4(ambient + diffuse + specular, 1.0);
}
