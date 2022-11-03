#ifndef MAIN_H
#define MAIN_H

#include <glad/glad.h>
#include <GLFW/glfw3.h>


extern "C" {
void set_target_img(const char* str);

void render_scene(struct scene *scene);
double get_mean_pixel_value();
void find_texture_difference();

int test_bed(double x, double y, double z, double pitch, double yaw, double roll, double roadWidth);

struct scene
{
	struct camera
	{
		double x;
		double y;
		double z;
		double pitch;
		double yaw;
		double roll;
	};
	double roadWidth;
	camera camera;
};
}

struct opengl_context
{
	GLFWwindow *window;
	//Shaders
	GLuint sceneShader;
	GLuint diffShader;
	GLuint outShader;
	GLuint computeShader;
	//Textures
	GLuint grassTexture;
	GLuint roadTexture;
	GLuint targetTexture;

	//Vertex Arrays -- Scene vertex arrays are not stored in the context
	GLuint diffVAO;
	GLuint diffVBO;
	GLuint diffEBO;
	GLuint outVAO;
	GLuint outVBO;
	GLuint outEBO;

	//Frame Buffers
	GLuint sceneFBO;
	GLuint diffFBO;

	GLuint ssbo;

	//Texture Buffers
	GLuint sceneTexture;
	GLuint diffTexture;
	// GLuint outTexture;
};

void init_context();
void framebuffer_size_callback(GLFWwindow *window, int width, int height);
GLFWwindow *init_gl_and_get_window();
void bind_scene_vertex_atts(GLuint VAO, GLuint VBO, GLuint EBO, float *vertices, GLuint vertices_length, GLuint *indices, GLuint indices_length);
void bind_diff_vertex_atts(GLuint VAO, GLuint VBO, GLuint EBO, float *vertices, GLuint vertices_length, GLuint *indices, GLuint indices_length);
void bind_texture(GLuint *texture, char *location);
void get_path(const char *target, char *dest);
void render_to_screen();
GLuint load_texture(const char *str);
void bind_frame_buffer(GLuint FBO, GLuint textureBuffer);
void terminate_context();
#endif