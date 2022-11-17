#version 330 core
out vec4 FragColor;

uniform int channel; 

void main()
{
	if (channel == 1) {
		FragColor = vec4(0, 1, 0, 1.0);
	} else if (channel == 0) {
		FragColor = vec4(1, 0, 0, 1.0);
	}
	
}
