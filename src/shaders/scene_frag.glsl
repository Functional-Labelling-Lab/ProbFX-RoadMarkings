#version 330 core
out vec4 FragColor;

in vec3 ourColor;

uniform int channel; 

void main()
{
	if (channel == 1) {
		FragColor = vec4(0.2157, 0.7765, 0.0471, 1.0);
	} else {
		FragColor =  vec4(ourColor, 1.0);
	}
	
}
