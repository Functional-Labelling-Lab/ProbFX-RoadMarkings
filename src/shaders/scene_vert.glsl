#version 330 core
layout (location = 0) in vec4 aPos;

out vec3 ourColor;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
	gl_Position = vec4(aPos);
	ourColor = vec3(0,0.1,0);
}