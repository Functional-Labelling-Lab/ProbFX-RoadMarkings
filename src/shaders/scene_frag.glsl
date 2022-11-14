#version 330 core
out vec4 FragColor;

in float ourChannel;

void main()
{
	if(ourChannel > 0.0)
	{
		FragColor = vec4(0.2157, 0.7765, 0.0471, 1.0);
	}
	else {
		FragColor = vec4(0,0,0,1.0);
	}
	
}
