#version 330 core
out vec4 FragColor;
  
in vec2 TexCoords;

uniform sampler2D texture1;
uniform sampler2D texture2;

void main()
{ 
	float r = abs(texture(texture2, TexCoords).x - texture(texture1, TexCoords).x);
	float g = abs(texture(texture2, TexCoords).y - texture(texture1, TexCoords).y);
	float b = abs(texture(texture2, TexCoords).z - texture(texture1, TexCoords).z);  
	float a = abs(texture(texture2, TexCoords).w - texture(texture1, TexCoords).w);  
	FragColor = vec4(r,g,b,a);
}
