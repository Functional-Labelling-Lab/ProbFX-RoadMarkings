#ifndef MAIN_H
#define MAIN_H

#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <string>
#include <glm/glm.hpp>

extern "C" {
void set_target_img(const char *str);

void render_scene(struct scene *scene, GLuint FBO);
double get_mean_pixel_value(GLuint texture, int color);
void get_image_mask(GLuint texture1, GLuint texture2, GLuint channel);
struct detected_lines *get_scene_geometry(struct scene *scene);

struct texture_fbo *create_texture_fbo();
GLuint get_scene_fbo();
GLuint get_target_texture();

struct texture_fbo {
  GLuint texture;
  GLuint frameBuffer;
};

int test_bed(const char *str, double x, double y, double z, double pitch, double yaw,
             double roll);

struct detected_lines *get_scene_geometry(struct scene *scene);

struct scene {
  struct camera {
    double x;
    double y;
    double z;
    double pitch;
    double yaw;
    double roll;
  };
  camera camera;
};
}

struct opengl_context {
  GLFWwindow *window;

  // Shaders
  GLuint sceneShader;
  GLuint diffShader;
  GLuint outShader;
  GLuint computeShader;

  // Textures
  GLuint grassTexture;
  GLuint roadTexture;
  GLuint targetTexture;

  // Vertex Arrays -- Scene vertex arrays are not stored in the context
  GLuint diffVAO;
  GLuint diffVBO;
  GLuint diffEBO;
  GLuint outVAO;
  GLuint outVBO;
  GLuint outEBO;

  // Frame Buffers
  GLuint sceneFBO;
  GLuint diffFBO;

  GLuint ssbo_diffs[2];
  int *ssbo_diff_maps[2];

  GLuint ssbo_nums[2];
  int *ssbo_num_maps[2];

  // Texture Buffers
  GLuint sceneTexture;
  GLuint diffTexture;
  // TODO: Why is this commented out?
  // GLuint outTexture;
};

void init_context();
GLFWwindow *init_gl_and_get_window();
void bind_scene_vertex_atts(GLuint VAO, GLuint VBO, GLuint EBO,
                            glm::vec4 *vertices, GLuint vertices_length,
                            GLuint *indices, GLuint indices_length);
void bind_diff_vertex_atts(GLuint VAO, GLuint VBO, GLuint EBO, float *vertices,
                           GLuint vertices_length, GLuint *indices,
                           GLuint indices_length);
void bind_texture(GLuint *texture, std::string &location);
std::string get_path(std::string &target);
void render_to_screen(GLuint texture);
GLuint load_texture(std::string str);
void bind_frame_buffer(GLuint FBO, GLuint textureBuffer);
void terminate_context();
#endif
