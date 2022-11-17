#version 330 core
layout (location = 0) in vec3 aPos;

out vec3 ourColor;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
	vec4 vert = projection * view * model * vec4(aPos, 1.0);
	float x = vert.x;
	float y = vert.y;
	float z = vert.y;
	float w = vert.w;
	float div = abs(w);
	gl_Position = vec4(x,y,z,w);
	ourColor = vec3(x,0,0);
}