#version 330 core
out vec4 FragColor;
  
in vec2 TexCoords;

uniform sampler2D mask;
uniform sampler2D texture;

void main()
{ 

	float r = abs(texture(texture, TexCoords).x - texture(mask, TexCoords).x);
	float g = abs(texture(texture, TexCoords).y - texture(mask, TexCoords).y);
	float b = abs(texture(texture, TexCoords).z - texture(mask, TexCoords).z);  
	float a = abs(texture(texture, TexCoords).w - texture(mask, TexCoords).w);  
	FragColor = vec4(r,g,b,1);
}
