#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <math.h>
#include <iostream>
#include <filesystem>
#include <string>
#include <sstream>
#include <fstream>
#include <ctime>

#include "shaders.h"
#include "main.h"

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#define STB_IMAGE_RESIZE_IMPLEMENTATION
#include "stb_image_resize.h"

#define _USE_MATH_DEFINES

void APIENTRY glDebugOutput(GLenum source,
														GLenum type,
														GLuint id,
														GLenum severity,
														GLsizei length,
														const char *message,
														const void *userParam);

// settings
const GLuint SCR_WIDTH = 1000;
const GLuint SCR_HEIGHT = 1000;

opengl_context *context = NULL;

int test_bed(double x, double y, double z, double pitch, double yaw, double roll, double roadWidth)
{
	struct scene scene;
	scene.roadWidth = roadWidth;
	scene.camera.x = x;
	scene.camera.y = y;
	scene.camera.z = z;
	scene.camera.pitch = pitch;
	scene.camera.yaw = yaw;
	scene.camera.roll = roll;

	// Seperate Load img function
	set_target_img("/src/textures/rendered_road2.jpg");

	while (!glfwWindowShouldClose(context->window))
	{
		std::clock_t start;

		start = std::clock();
		// Test vars uncomment to move scene
		// scene.camera.pitch -= 0.0003;
		// scene.camera.x -= 0.001;

		// Renders into sceneFBO where the texture is in sceneTexture
		render_scene(&scene);

		// Renders into diffFBO where the texture is in diffTexture
		find_texture_difference();

		// Render to screen for visual debugging
		render_to_screen();

		// Calculate Error
		// uncomment if you wanna be spammed in the terminal
		std::cout << get_mean_pixel_value() << std::endl;
		// get_mean_pixel_value();
		// float pv = get_mean_pixel_value();

		// This is just for local rending
		// glfwSwapBuffers(context->window);
		// glfwPollEvents();
		std::cout << "Time: " << (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000) << " ms" << std::endl;

		// break;
	}
	terminate_context();

	return 0;
}

void init_context()
{
	context = new opengl_context;

	context->window = init_gl_and_get_window();

	// Load all shaders
	// scene shader
	context->sceneShader = load_shader("/src/shaders/scene_vert.glsl", "/src/shaders/scene_frag.glsl");
	// diff shader
	context->diffShader = load_shader("/src/shaders/diff_vert.glsl", "/src/shaders/diff_frag.glsl");
	// out shader
	context->outShader = load_shader("/src/shaders/out_vert.glsl", "/src/shaders/out_frag.glsl");

	float diff_vertices[] = {
			// positions   // texture coords
			1.0f, 1.0f, 1.0f, 1.0f,		// top right
			1.0f, -1.0f, 1.0f, 0.0f,	// bottom right
			-1.0f, -1.0f, 0.0f, 0.0f, // bottom left
			-1.0f, 1.0f, 0.0f, 1.0f		// top left
	};

	GLuint diff_indices[] = {
			0, 1, 3, // first triangle
			1, 2, 3	 // second triangle
	};

	float out_vertices[] = {
			// positions   // texture coords
			1.0f, 1.0f, 1.0f, 1.0f,		// top right
			1.0f, -1.0f, 1.0f, 0.0f,	// bottom right
			-1.0f, -1.0f, 0.0f, 0.0f, // bottom left
			-1.0f, 1.0f, 0.0f, 1.0f		// top left
	};

	GLuint out_indices[] = {
			0, 1, 3, // first triangle
			1, 2, 3	 // second triangle
	};

	// Bind buffers
	// Take size from struct todo
	glGenVertexArrays(1, &context->diffVAO);
	glGenBuffers(1, &context->diffVBO);
	glGenBuffers(1, &context->diffEBO);
	glGenVertexArrays(1, &context->outVAO);
	glGenBuffers(1, &context->outVBO);
	glGenBuffers(1, &context->outEBO);
	bind_diff_vertex_atts(context->diffVAO, context->diffVBO, context->diffEBO, diff_vertices, sizeof(diff_vertices), diff_indices, sizeof(diff_indices));
	bind_diff_vertex_atts(context->outVAO, context->outVBO, context->outEBO, diff_vertices, sizeof(diff_vertices), diff_indices, sizeof(diff_indices));

	// Load textures
	context->grassTexture = load_texture("/src/textures/grass.jpg");
	context->roadTexture = load_texture("/src/textures/road.jpg");

	// Framebuffer configs
	// Take size from struct todo
	glGenFramebuffers(1, &context->sceneFBO);
	glGenTextures(1, &context->sceneTexture);
	glGenFramebuffers(1, &context->diffFBO);
	glGenTextures(1, &context->diffTexture);
	// glCreateTextures(GL_TEXTURE_2D, 1, &context->diffTexture);

	// Stage one buffer (Scene)
	bind_frame_buffer(context->sceneFBO, context->sceneTexture);

	// Stage two buffer (image_diffrence)
	bind_frame_buffer(context->diffFBO, context->diffTexture);

	GLuint ssbo;
	glGenBuffers(1, &ssbo);

	context->ssbo = ssbo;

	// glBindBuffer(GL_SHADER_STORAGE_BUFFER, 1);
	// glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, 1);
	// glBufferData(GL_SHADER_STORAGE_BUFFER, sizeof(size_t), 0, GL_DYNAMIC_COPY);

	// glBindBuffer(GL_SHADER_STORAGE_BUFFER, 1);
	// Load compute shader into memory
	std::string ComputeShaderCode;
	std::ifstream ComputeShaderStream("./src/shaders/mse.computeshader", std::ios::in);
	if (ComputeShaderStream.is_open())
	{
		std::stringstream sstr;
		sstr << ComputeShaderStream.rdbuf();
		ComputeShaderCode = sstr.str();
		ComputeShaderStream.close();
	}
	// Compile the compute shader
	char *prog = &ComputeShaderCode[0];
	GLuint mse_shader = glCreateShader(GL_COMPUTE_SHADER);
	glShaderSource(mse_shader, 1, &prog, NULL);
	glCompileShader(mse_shader);
	// Link the compute shader
	GLuint mse_program = glCreateProgram();
	glAttachShader(mse_program, mse_shader);
	glLinkProgram(mse_program);

	context->computeShader = mse_program;

	// uncomment this call to draw in wireframe polygons.
	// glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
}

void bind_frame_buffer(GLuint FBO, GLuint textureBuffer)
{
	glBindFramebuffer(GL_FRAMEBUFFER, FBO);
	glBindTexture(GL_TEXTURE_2D, textureBuffer);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, textureBuffer, 0);

	// Switch to screen output buffer for safety to stop anything else being rendered to the FBO
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

GLuint load_texture(const char *str)
{
	GLuint texture;
	char texturePath[100];
	get_path(str, texturePath);
	bind_texture(&texture, texturePath);
	return texture;
}

void render_to_screen()
{
	if (!context)
	{
		init_context();
	}
	// Switch to screen output buffer
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	glClearColor(0.3f, 0.3f, 0.3f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);
	// Links texture locations to shaders (I think)
	glUseProgram(context->outShader);
	glUniform1i(glGetUniformLocation(context->outShader, "ourTexture"), 0);
	// Load diff texture
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, context->diffTexture);
	// Draw it to whole screen
	glBindVertexArray(context->outVAO);
	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
	glfwSwapBuffers(context->window);
	glfwPollEvents();
}

void find_texture_difference()
{
	if (!context)
	{
		init_context();
	}
	// Switch to difference test_bed buffer and clear it
	glBindFramebuffer(GL_FRAMEBUFFER, context->diffFBO);
	glClearColor(0.3f, 0.3f, 0.3f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);

	// Links texture locations to shaders (I think)
	glUseProgram(context->diffShader);
	glUniform1i(glGetUniformLocation(context->diffShader, "texture1"), 0);
	glUniform1i(glGetUniformLocation(context->diffShader, "texture2"), 1);

	// Load textures
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, context->sceneTexture);
	glActiveTexture(GL_TEXTURE1);
	glBindTexture(GL_TEXTURE_2D, context->targetTexture);

	// Get image difference and draw it to whole buffer
	glBindVertexArray(context->diffVAO);
	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

	// Switch to screen output buffer for safety to stop anything else being rendered to the FBO
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	render_to_screen();
}

void set_target_img(const char *str)
{
	if (!context)
	{
		init_context();
	}
	context->targetTexture = load_texture(str);
}

void render_scene(struct scene *scene)
{
	if (!context)
	{
		init_context();
	}
	// std::cout << "Rendering scene" << std::endl;
	// std::cout << scene->roadWidth << std::endl;
	float planeSize = 100.0f;
	float scaleDown = 2;

	// This is done here because it depends on the scene
	float ground_vertices[] = {
			// positions                   // colors          // texture coords
			planeSize, 0.0f, planeSize, 0.5f, 0.8f, 0.5f, scaleDown * planeSize * 1.0f, scaleDown * planeSize * 1.0f, // top right
			planeSize, 0.0f, -planeSize, 0.5f, 0.8f, 0.5f, scaleDown * planeSize * 1.0f, 0.0f,												// bottom right
			-planeSize, 0.0f, -planeSize, 0.5f, 0.8f, 0.5f, 0.0f, 0.0f,																								// bottom left
			-planeSize, 0.0f, planeSize, 0.5f, 0.8f, 0.5f, 0.0f, scaleDown * planeSize * 1.0f													// top left
	};

	GLuint ground_indices[] = {
			0, 1, 3, // first triangle
			1, 2, 3	 // second triangle
	};

	float roadWidth = 0.3;
	float road_vertices[] = {
			// positions                        // colors         // texture coords
			static_cast<float>(scene->roadWidth) / 2, 0.0f, planeSize, 0.2f, 0.2f, 0.2f, 1.0f, scaleDown * planeSize * 1.0f, // top right
			static_cast<float>(scene->roadWidth) / 2, 0.0f, -planeSize, 0.2f, 0.2f, 0.2f, 1.0f, 0.0f,												 // bottom right
			-static_cast<float>(scene->roadWidth) / 2, 0.0f, -planeSize, 0.2f, 0.2f, 0.2f, 0.0f, 0.0f,											 // bottom left
			-static_cast<float>(scene->roadWidth) / 2, 0.0f, planeSize, 0.2f, 0.2f, 0.2f, 0.0f, scaleDown * planeSize * 1.0f // top left
	};

	GLuint road_indices[] = {
			0, 1, 3, // first trianglebind_diff_vertex_atts
			1, 2, 3	 // second triangle
	};

	GLuint VAOs[2], VBOs[2], EBOs[2];
	glGenVertexArrays(2, VAOs);
	glGenBuffers(2, VBOs);
	glGenBuffers(2, EBOs);
	bind_scene_vertex_atts(VAOs[0], VBOs[0], EBOs[0], ground_vertices, sizeof(ground_vertices), ground_indices, sizeof(ground_indices));
	bind_scene_vertex_atts(VAOs[1], VBOs[1], EBOs[1], road_vertices, sizeof(road_vertices), road_indices, sizeof(road_indices));

	glm::mat4 model, view, projection;
	glm::vec3 cameraPos, cameraTarget;
	int modelLoc, viewLoc, projectionLoc;

	// Bind to framebuffer so images displayed there rather than screen
	glBindFramebuffer(GL_FRAMEBUFFER, context->sceneFBO);

	// Clear buffer
	glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);

	// Start using the screenShader shaders and link texture
	glUseProgram(context->sceneShader);
	glUniform1i(glGetUniformLocation(context->sceneShader, "ourTexture"), 0);

	// Model Tranformations -- None atm so commented out
	model = glm::mat4(1.0f);
	// model = glm::rotate(model, glm::radians(0.0f), glm::vec3(1.0f, 0.0f, 0.0f));

	// View Transformation -- Calculated by picking a position and a point to look at
	cameraPos = glm::vec3(-scene->camera.x, scene->camera.y, scene->camera.z);
	cameraTarget = glm::vec3(-scene->camera.x, scene->camera.y + sin((M_PI / 2) * scene->camera.pitch), scene->camera.z - cos((M_PI / 2) * scene->camera.pitch));
	view = glm::lookAt(cameraPos, cameraTarget, glm::vec3(0.0, 1.0, 0.0));

	// Projection -- Sets perspective (FOV 45 degrees and in perspective projection)
	projection = glm::perspective(glm::radians(45.0f), (float)SCR_WIDTH / (float)SCR_HEIGHT, 0.1f, 100.0f);

	// Links up Model, View and Projection matrix with shaders
	modelLoc = glGetUniformLocation(context->sceneShader, "model");
	glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
	viewLoc = glGetUniformLocation(context->sceneShader, "view");
	glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view));
	projectionLoc = glGetUniformLocation(context->sceneShader, "projection");
	glUniformMatrix4fv(projectionLoc, 1, GL_FALSE, glm::value_ptr(projection));

	// Render ground
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, context->grassTexture);
	glBindVertexArray(VAOs[0]);
	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

	// Render road
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, context->roadTexture);
	glBindVertexArray(VAOs[1]);
	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

	// Switch to screen output buffer for safety to stop anything else being rendered to the FBO
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

// float get_mean_pixel_value()
// {
// 	// std::clock_t    start;

// 	// start = std::clock();
// 	int texW = SCR_WIDTH;
// 	int texH = SCR_HEIGHT;
// 	GLuint texture = context->diffTexture;

// 	// Load Texture
// 	glActiveTexture(GL_TEXTURE0);
// 	glBindTexture(GL_TEXTURE_2D, texture);

// 	// Validate correctness later
// 	glGenerateMipmap(GL_TEXTURE_2D);

// 	using namespace std;
// 	// Formula from the glspec, "Mipmapping" subsection in section 3.8.11 Texture Minification
// 	const auto totalMipmapLevels = 1 + floor(log2(max(texW, texH)));
// 	const auto deepestLevel = totalMipmapLevels - 1;

// 	// Sanity check
// 	int deepestMipmapLevelWidth = -1, deepestMipmapLevelHeight = -1;
// 	glGetTexLevelParameteriv(GL_TEXTURE_2D, deepestLevel, GL_TEXTURE_WIDTH, &deepestMipmapLevelWidth);
// 	glGetTexLevelParameteriv(GL_TEXTURE_2D, deepestLevel, GL_TEXTURE_HEIGHT, &deepestMipmapLevelHeight);
// 	assert(deepestMipmapLevelWidth == 1);
// 	assert(deepestMipmapLevelHeight == 1);

// 	glm::vec4 pixel;
// 	glGetTexImage(GL_TEXTURE_2D, deepestLevel, GL_RGBA, GL_FLOAT, &pixel[0]);

// 	// std::cerr << "Mipmap value: " << pixel[0] << ", " << pixel[1] << ", " << pixel[2] << ", " << pixel[3] << "\n";
// 	// std::cout << "Time: " << (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000) << " ms" << std::endl;
// 	glfwSwapBuffers(context->window);
// 	glfwPollEvents();
// 	return pixel[0] + pixel[1] + pixel[2];
// }

void get_path(const char *target, char *dest)
{
	std::string stringCurrentPath = std::filesystem::current_path().string();
	char *currentPath = new char[stringCurrentPath.length() + 1];
	strcpy(currentPath, stringCurrentPath.c_str());
	strcpy(dest, currentPath);
	strcat(dest, target);
}

GLFWwindow *init_gl_and_get_window()
{
	// glfw: initialize and configure
	glfwInit();
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
	glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, true);

	// glfw window creation
	GLFWwindow *window = glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, "OpenGL", NULL, NULL);
	if (window == NULL)
	{
		std::cout << "Failed to create GLFW window" << std::endl;
		glfwTerminate();
	}

	glfwMakeContextCurrent(window);
	glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);

	// Stops textures rendering upsidedown
	stbi_set_flip_vertically_on_load(true);

	// glad: load all OpenGL function pointers
	if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
	{
		std::cout << "Failed to initialize GLAD" << std::endl;
	}

	int flags;
	glGetIntegerv(GL_CONTEXT_FLAGS, &flags);
	if (flags & GL_CONTEXT_FLAG_DEBUG_BIT)
	{
		glEnable(GL_DEBUG_OUTPUT);
		glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
		glDebugMessageCallback(glDebugOutput, nullptr);
		glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nullptr, GL_TRUE);
	}
	else
	{
		std::cout << "We don't have debugging" << std::endl;
	}
	return window;
}

void bind_texture(GLuint *texture, char *location)
{
	glGenTextures(1, texture);
	glBindTexture(GL_TEXTURE_2D, *texture);
	// set the texture wrapping/filtering options (on the currently bound texture object)
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	// Load image
	int width, height, nrChannels;
	unsigned char *data = stbi_load(location, &width, &height, &nrChannels, 0);
	if (data)
	{
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
		glGenerateMipmap(GL_TEXTURE_2D);
	}
	else
	{
		std::cout << "Failed to load texture" << std::endl;
	}
	stbi_image_free(data);
}

void bind_diff_vertex_atts(GLuint VAO, GLuint VBO, GLuint EBO, float *vertices, GLuint vertices_length, GLuint *indices, GLuint indices_length)
{
	glBindVertexArray(VAO);

	glBindBuffer(GL_ARRAY_BUFFER, VBO);
	glBufferData(GL_ARRAY_BUFFER, vertices_length, vertices, GL_STATIC_DRAW);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices_length, indices, GL_STATIC_DRAW);

	// Position 2D
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void *)0);
	glEnableVertexAttribArray(0);

	// Texture coord
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void *)(2 * sizeof(float)));
	glEnableVertexAttribArray(1);

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
}

void bind_scene_vertex_atts(GLuint VAO, GLuint VBO, GLuint EBO, float *vertices, GLuint vertices_length, GLuint *indices, GLuint indices_length)
{
	glBindVertexArray(VAO);

	glBindBuffer(GL_ARRAY_BUFFER, VBO);
	glBufferData(GL_ARRAY_BUFFER, vertices_length, vertices, GL_STATIC_DRAW);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices_length, indices, GL_STATIC_DRAW);

	// Position 3D
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void *)0);
	glEnableVertexAttribArray(0);

	// Colour
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void *)(3 * sizeof(float)));
	glEnableVertexAttribArray(1);

	// Texture coord
	glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void *)(6 * sizeof(float)));
	glEnableVertexAttribArray(2);

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
}

void framebuffer_size_callback(GLFWwindow *window, int width, int height)
{
	// make sure the viewport matches the new window dimensions; note that width and
	// height will be significantly larger than specified on retina displays.
	glViewport(0, 0, width, height);
}
void terminate_context()
{
	// Do i need to clean up textures, window as well (I assume so will look into this later)

	glDeleteVertexArrays(1, &context->diffVAO);
	glDeleteBuffers(1, &context->diffVBO);
	glDeleteBuffers(1, &context->diffEBO);

	glDeleteVertexArrays(1, &context->outVAO);
	glDeleteBuffers(1, &context->outVBO);
	glDeleteBuffers(1, &context->outEBO);

	glDeleteBuffers(1, &context->sceneFBO);
	glDeleteBuffers(1, &context->diffFBO);
	glfwTerminate();
}

double get_mean_pixel_value()
{

	// Get average value of the rendered pixels as the value of the deepest mipmap level
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, context->targetTexture);
	glGenerateMipmap(GL_TEXTURE_2D);

	// Checking the size of the pixel
	int deepestMipmapLevelWidth = -1, deepestMipmapLevelHeight = -1;
	glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &deepestMipmapLevelWidth);
	glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &deepestMipmapLevelHeight);
	std::cout << deepestMipmapLevelWidth << std::endl;
	std::cout << deepestMipmapLevelHeight << std::endl;
	// glGetTexImage(GL_TEXTURE_2D, deepestLevel, GL_RGBA, GL_FLOAT, &pixel[0]);

	// Times by 3 for RGB
	GLfloat *pixels = new GLfloat[SCR_WIDTH * SCR_HEIGHT * 3];

	glGetTexImage(GL_TEXTURE_2D, 0, GL_RGB, GL_FLOAT, pixels);

	GLfloat cumR = 0;
	GLfloat cumG = 0;
	GLfloat cumB = 0;
	for (int x = 0; x < SCR_WIDTH; x++)
	{
		for (int y = 0; y < SCR_HEIGHT; y++)
		{
			cumR += pixels[x*3 + y*SCR_WIDTH + 0];
			cumG += pixels[x*3 + y*SCR_WIDTH + 1];
			cumB += pixels[x*3 + y*SCR_WIDTH + 2];
		}
	}
	std::cout << cumR + cumG + cumB << std::endl;	
	return cumR + cumG + cumB;
}

void APIENTRY glDebugOutput(GLenum source,
														GLenum type,
														GLuint id,
														GLenum severity,
														GLsizei length,
														const char *message,
														const void *userParam)
{
	// ignore non-significant error/warning codes
	if (id == 131169 || id == 131185 || id == 131218 || id == 131204)
		return;

	std::cout << "---------------" << std::endl;
	std::cout << "Debug message (" << id << "): " << message << std::endl;

	switch (source)
	{
	case GL_DEBUG_SOURCE_API:
		std::cout << "Source: API";
		break;
	case GL_DEBUG_SOURCE_WINDOW_SYSTEM:
		std::cout << "Source: Window System";
		break;
	case GL_DEBUG_SOURCE_SHADER_COMPILER:
		std::cout << "Source: Shader Compiler";
		break;
	case GL_DEBUG_SOURCE_THIRD_PARTY:
		std::cout << "Source: Third Party";
		break;
	case GL_DEBUG_SOURCE_APPLICATION:
		std::cout << "Source: Application";
		break;
	case GL_DEBUG_SOURCE_OTHER:
		std::cout << "Source: Other";
		break;
	}
	std::cout << std::endl;

	switch (type)
	{
	case GL_DEBUG_TYPE_ERROR:
		std::cout << "Type: Error";
		break;
	case GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR:
		std::cout << "Type: Deprecated Behaviour";
		break;
	case GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR:
		std::cout << "Type: Undefined Behaviour";
		break;
	case GL_DEBUG_TYPE_PORTABILITY:
		std::cout << "Type: Portability";
		break;
	case GL_DEBUG_TYPE_PERFORMANCE:
		std::cout << "Type: Performance";
		break;
	case GL_DEBUG_TYPE_MARKER:
		std::cout << "Type: Marker";
		break;
	case GL_DEBUG_TYPE_PUSH_GROUP:
		std::cout << "Type: Push Group";
		break;
	case GL_DEBUG_TYPE_POP_GROUP:
		std::cout << "Type: Pop Group";
		break;
	case GL_DEBUG_TYPE_OTHER:
		std::cout << "Type: Other";
		break;
	}
	std::cout << std::endl;

	switch (severity)
	{
	case GL_DEBUG_SEVERITY_HIGH:
		std::cout << "Severity: high";
		break;
	case GL_DEBUG_SEVERITY_MEDIUM:
		std::cout << "Severity: medium";
		break;
	case GL_DEBUG_SEVERITY_LOW:
		std::cout << "Severity: low";
		break;
	case GL_DEBUG_SEVERITY_NOTIFICATION:
		std::cout << "Severity: notification";
		break;
	}
	std::cout << std::endl;
	std::cout << std::endl;
}