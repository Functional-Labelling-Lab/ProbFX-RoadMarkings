#version 330 core
out vec4 FragColor;
  
in vec2 TexCoords;

uniform int channel;
uniform sampler2D mask;
uniform sampler2D myTexture;

void main()
{ 
	if (channel == 0) {
		if (texture(mask, TexCoords).x == 1) {
			FragColor = texture(myTexture, TexCoords);
		} else {
			FragColor = vec4(0,0,0,1);
		}
	} else if (channel == 1) {
		if (texture(mask, TexCoords).y == 1) {
			FragColor = texture(myTexture, TexCoords);
		} else {
			FragColor = vec4(0,0,0,1);
		}
	} else {
		FragColor = vec4(0,0,0,1);
	}
}
