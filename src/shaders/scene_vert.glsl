#version 330 core
layout (location = 0) in vec4 aPos;

out vec3 ourColor;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
	vec4 vert = vec4(aPos);
	float x = vert.x;
	float y = vert.y;
	float z = vert.y;
	float w = vert.w;
	float div = abs(w);
	gl_Position = vec4(x,y,z,w);
	ourColor = vec3(0,0.1,0);
}