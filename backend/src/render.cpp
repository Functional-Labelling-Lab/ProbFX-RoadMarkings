#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <bits/stdc++.h>

#define SHADER_CONSTS
#include "shaders.h"
#include "hough.h"
#include "render.h"

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#define STB_IMAGE_RESIZE_IMPLEMENTATION
#include "stb_image_resize.h"

#define _USE_MATH_DEFINES

void APIENTRY glDebugOutput(GLenum source, GLenum type, GLuint id,
                            GLenum severity, GLsizei length,
                            const char *message, const void *userParam);

// settings
const GLuint SCR_WIDTH = 1000;
const GLuint SCR_HEIGHT = 1000;

opengl_context *context = NULL;

const char *MSE_COMPUTE_SHADER =
#include "shaders/mse.computeshader"
    ;

int test_bed(const char *str, double x, double y, double z, double pitch, double yaw,
             double roll) {
  struct scene scene;
  scene.camera.x = x;
  scene.camera.y = y;
  scene.camera.z = z;
  scene.camera.pitch = pitch;
  scene.camera.yaw = yaw;
  scene.camera.roll = roll;

  // Seperate Load img function
  set_target_img(str);

  while (!glfwWindowShouldClose(context->window)) {
    for (int i = 0; i < 120; i++) {
      render_to_screen(context->targetTexture);
    }
    // Renders into sceneFBO where the texture is in sceneTexture
    render_scene(&scene);

    for (int i = 0; i < 120; i++) {
      render_to_screen(context->sceneTexture);
    }
    // Renders into diffFBO where the texture is in diffTexture
    get_image_mask(context->sceneTexture, context->targetTexture, 0);
    double error1 = get_mean_pixel_value(context->diffTexture, 0);
    std::cout << error1 << std::endl;
    for (int i = 0; i < 120; i++) {
      render_to_screen(context->diffTexture);
    }
    get_image_mask(context->sceneTexture, context->targetTexture, 1);
    double error2 = get_mean_pixel_value(context->diffTexture, 1);
    std::cout << error2 << std::endl;
    for (int i = 0; i < 120; i++) {
      render_to_screen(context->diffTexture);
    }
    get_image_mask(context->sceneTexture, context->targetTexture, 2);
    double error3 = get_mean_pixel_value(context->diffTexture, 2);
    std::cout << error3 << std::endl;
    for (int i = 0; i < 120; i++) {
      render_to_screen(context->diffTexture);
    }

    // Render to screen for visual debugging

    // TODO: Sort this out:
    // Calculate Error
    // uncomment if you wanna be spammed in the terminal
    // std::cout << error1 + error2 + error3 << std::endl;

    std::cout << std::endl << std::endl;

    // break;
  }
  terminate_context();

  return 0;
}

void init_context() {
#ifdef OGL4
  std::cout << "Using OpenGL 4" << std::endl;
#else
  std::cout << "Using OpenGL 3" << std::endl;
#endif

  context = new opengl_context;
  context->window = init_gl_and_get_window();

  // Load all shaders
  context->sceneShader = load_shader(scene_vertex, scene_fragment);
  context->diffShader = load_shader(diff_vertex, diff_fragment);
  context->outShader = load_shader(out_vertex, out_fragment);

  // For drawing onto a box the fits the whole screen to show our texture
  float diff_vertices[] = {
      // positions   // texture coords
      1.0f,  1.0f,  1.0f, 1.0f, // top right
      1.0f,  -1.0f, 1.0f, 0.0f, // bottom right
      -1.0f, -1.0f, 0.0f, 0.0f, // bottom left
      -1.0f, 1.0f,  0.0f, 1.0f  // top left
  };

  GLuint diff_indices[] = {
      0, 1, 3, // first triangle
      1, 2, 3  // second triangle
  };

  float out_vertices[] = {
      // positions   // texture coords
      1.0f,  1.0f,  1.0f, 1.0f, // top right
      1.0f,  -1.0f, 1.0f, 0.0f, // bottom right
      -1.0f, -1.0f, 0.0f, 0.0f, // bottom left
      -1.0f, 1.0f,  0.0f, 1.0f  // top left
  };

  GLuint out_indices[] = {
      0, 1, 3, // first triangle
      1, 2, 3  // second triangle
  };

  // Bind buffers
  // TODO: Take size from struct
  glGenVertexArrays(1, &context->diffVAO);
  glGenBuffers(1, &context->diffVBO);
  glGenBuffers(1, &context->diffEBO);
  glGenVertexArrays(1, &context->outVAO);
  glGenBuffers(1, &context->outVBO);
  glGenBuffers(1, &context->outEBO);
  bind_diff_vertex_atts(context->diffVAO, context->diffVBO, context->diffEBO,
                        diff_vertices, sizeof(diff_vertices), diff_indices,
                        sizeof(diff_indices));
  bind_diff_vertex_atts(context->outVAO, context->outVBO, context->outEBO,
                        diff_vertices, sizeof(diff_vertices), diff_indices,
                        sizeof(diff_indices));

  // Framebuffer configs
  // TODO: Take size from struct
  glGenFramebuffers(1, &context->sceneFBO);
  glGenTextures(1, &context->sceneTexture);
  glGenFramebuffers(1, &context->diffFBO);
  glGenTextures(1, &context->diffTexture);

  // Stage one buffer (Scene)
  bind_frame_buffer(context->sceneFBO, context->sceneTexture);

  // Stage two buffer (image_diffrence)
  bind_frame_buffer(context->diffFBO, context->diffTexture);

#ifdef OGL4
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);

  GLuint mse_shader = glCreateShader(GL_COMPUTE_SHADER);
  glShaderSource(mse_shader, 1, &MSE_COMPUTE_SHADER, NULL);
  glCompileShader(mse_shader);
  GLsizei log_length = 0;
  GLchar message[1024];
  glGetShaderInfoLog(mse_shader, 1024, &log_length, message);
  // Link the compute shader
  GLuint mse_program = glCreateProgram();
  glAttachShader(mse_program, mse_shader);
  glLinkProgram(mse_program);

  GLuint ssbo_diffs[2];
  glGenBuffers(2, ssbo_diffs);

  GLuint ssbo_nums[2];
  glGenBuffers(2, ssbo_nums);

  for (int i = 0; i < 2; i++) {
    context->ssbo_diffs[i] = ssbo_diffs[i];
    // Bind it to the GL_SHADER_STORAGE_BUFFER target.
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, context->ssbo_diffs[i]);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, context->ssbo_diffs[i]);

    glBufferStorage(GL_SHADER_STORAGE_BUFFER,             // target
                    sizeof(int) * SCR_HEIGHT * SCR_WIDTH, // total size
                    NULL,                                 // no data
                    GL_MAP_READ_BIT | GL_MAP_PERSISTENT_BIT |
                        GL_MAP_COHERENT_BIT);
    context->ssbo_diff_maps[i] = (int *)glMapBufferRange(
        GL_SHADER_STORAGE_BUFFER, 0, sizeof(int),
        GL_MAP_READ_BIT | GL_MAP_PERSISTENT_BIT | GL_MAP_COHERENT_BIT);

    context->ssbo_nums[i] = ssbo_nums[i];
    // Bind it to the GL_SHADER_STORAGE_BUFFER target.
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, context->ssbo_nums[i]);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, context->ssbo_nums[i]);

    glBufferStorage(GL_SHADER_STORAGE_BUFFER,             // target
                    sizeof(int) * SCR_HEIGHT * SCR_WIDTH, // total size
                    NULL,                                 // no data
                    GL_MAP_READ_BIT | GL_MAP_PERSISTENT_BIT |
                        GL_MAP_COHERENT_BIT);
    context->ssbo_num_maps[i] = (int *)glMapBufferRange(
        GL_SHADER_STORAGE_BUFFER, 0, sizeof(int),
        GL_MAP_READ_BIT | GL_MAP_PERSISTENT_BIT | GL_MAP_COHERENT_BIT);
  }

  std::cout << message << std::endl;
  // TODO:
  // exit(1);

  context->computeShader = mse_program;
#else
#endif

  // TODO:
  // uncomment this call to draw in wireframe polygons.
  // glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
}

void bind_frame_buffer(GLuint FBO, GLuint textureBuffer) {
  glBindFramebuffer(GL_FRAMEBUFFER, FBO);
  glBindTexture(GL_TEXTURE_2D, textureBuffer);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGB,
               GL_UNSIGNED_BYTE, NULL);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,
                         textureBuffer, 0);

  // Switch to screen output buffer for safety to stop anything else being
  // rendered to the FBO
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

GLuint load_texture(std::string str) {
  GLuint texture;
  std::string texturePath = get_path(str);
  bind_texture(&texture, texturePath);
  return texture;
}

void render_to_screen(GLuint texture) {
  // Switch to screen output buffer
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);
  // Links texture locations to shaders (I think)
  glUseProgram(context->outShader);
  glUniform1i(glGetUniformLocation(context->outShader, "ourTexture"), 0);
  // Load diff texture
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, texture);
  // Draw it to whole screen
  glBindVertexArray(context->outVAO);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
  glfwSwapBuffers(context->window);
  glfwPollEvents();
}

void get_image_mask(GLuint texture1, GLuint texture2, GLuint channel) {

  // Switch to difference test_bed buffer and clear it
  glBindFramebuffer(GL_FRAMEBUFFER, context->diffFBO);
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);

  // Links texture locations to shaders (I think)
  glUseProgram(context->diffShader);
  glUniform1i(glGetUniformLocation(context->diffShader, "channel"), channel);
  glUniform1i(glGetUniformLocation(context->diffShader, "mask"), 0);
  glUniform1i(glGetUniformLocation(context->diffShader, "myTexture"), 1);

  // Load textures
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, texture1);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, texture2);

  // Get image difference and draw it to whole buffer
  glBindVertexArray(context->diffVAO);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

  // Switch to screen output buffer for safety to stop anything else being
  // rendered to the FBO
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void set_target_img(const char *str) {
  context->targetTexture = load_texture(str);
}

void generate_rendering_matrices(glm::mat4* model, glm::mat4* view, glm::mat4* projection, struct scene *scene){
	glm::vec3 cameraPos, cameraTarget;
	  // TODO:
  // Model Tranformations -- None atm so commented out
  *model = glm::mat4(1.0f);
  // model = glm::rotate(model, glm::radians(90.0f * (float)
  // scene->camera.roll), glm::vec3(0.0f, 0.0f, 1.0f));

  // View Transformation -- Calculated by picking a position and a point to look
  // at
  cameraPos = glm::vec3(-scene->camera.x, scene->camera.y, scene->camera.z);

  cameraTarget =
      glm::vec3(-scene->camera.x + sin((M_PI / 2) * scene->camera.yaw),
                scene->camera.y + sin((M_PI / 2) * scene->camera.pitch *
                                      cos((M_PI / 2) * scene->camera.yaw)),
                scene->camera.z - cos((M_PI / 2) * scene->camera.pitch));
  *view = glm::lookAt(cameraPos, cameraTarget,
                     glm::vec3(sin((M_PI / 2) * scene->camera.roll),
                               cos((M_PI / 2) * scene->camera.roll), 0.0));

  // Projection -- Sets perspective (FOV 45 degrees and in perspective
  // projection)
  *projection = glm::perspective(
      glm::radians(45.0f), (float)SCR_WIDTH / (float)SCR_HEIGHT, 0.1f, 100.0f);
	return;
}


struct detected_lines *get_scene_geometry(struct scene *scene){
	glm::mat4 model, view, projection;
	generate_rendering_matrices(&model, &view, &projection, scene);
	
	glm::mat4 combMatrix = projection * view * model; 

	//Needs to be refactored so generated the same way as render_scene
	float roadWidth = 0.15;
	float planeSize = 100.0f;
  glm::vec4 road_vertices[] = {
      // positions
      glm::vec4(roadWidth, 0.0f, planeSize,1),   // top right
      glm::vec4(roadWidth, 0.0f, -planeSize,1),  // bottom right
      glm::vec4(-roadWidth, 0.0f, -planeSize,1), // bottom left
      glm::vec4(-roadWidth, 0.0f, planeSize,1)   // top left
  };

	for (int i = 0; i < 4; i++) {
		road_vertices[i] = combMatrix * road_vertices[i];
	}

	//Magic to turn screen space coordinates
	// atm it's divide by abs(4) i.e road_vertices[i][4] but i need to clip after

	struct detected_lines* geometry_lines = (detected_lines*) malloc(sizeof(detected_lines));
	geometry_lines->len = 2; 
	geometry_lines->lines = (hough_line*) malloc(sizeof(hough_line)*2);
	geometry_lines->lines[0].startX = road_vertices[0][0];
	geometry_lines->lines[0].endX = road_vertices[0][1];
	geometry_lines->lines[0].startY = road_vertices[1][0];
	geometry_lines->lines[0].endY = road_vertices[1][1];
	geometry_lines->lines[1].startX = road_vertices[2][0];
	geometry_lines->lines[1].endX = road_vertices[2][1];
	geometry_lines->lines[1].startY = road_vertices[3][0];
	geometry_lines->lines[1].endY = road_vertices[3][1];

	return geometry_lines;
}

void render_scene(struct scene *scene) {
  float planeSize = 100.0f;

  // This is done here because it depends on the scene
  glm::vec4 ground_vertices[] = {
      // positions
      glm::vec4(planeSize, 0.0f, planeSize,1.0),   // top right
      glm::vec4(planeSize, 0.0f, -planeSize,1.0),  // bottom right
      glm::vec4(-planeSize, 0.0f, -planeSize,1.0), // bottom left
      glm::vec4(-planeSize, 0.0f, planeSize,1.0)   // top left
  };

  GLuint ground_indices[] = {
      0, 1, 3, // first triangle
      1, 2, 3  // second triangle
  };

  float roadWidth = 0.15;
  glm::vec4 road_vertices[] = {
      // positions
      glm::vec4(roadWidth, 0.0f, planeSize,1.0),   // top right
      glm::vec4(roadWidth, 0.0f, -planeSize,1.0),  // bottom right
      glm::vec4(-roadWidth, 0.0f, -planeSize,1.0), // bottom left
      glm::vec4(-roadWidth, 0.0f, planeSize,1.0)   // top left
  };

  GLuint road_indices[] = {
      0, 1, 3, // first trianglebind_diff_vertex_atts
      1, 2, 3  // second triangle
  };

  GLuint VAOs[2], VBOs[2], EBOs[2];
  glGenVertexArrays(2, VAOs);
  glGenBuffers(2, VBOs);
  glGenBuffers(2, EBOs);
  bind_scene_vertex_atts(VAOs[0], VBOs[0], EBOs[0], ground_vertices,
                         sizeof(ground_vertices), ground_indices,
                         sizeof(ground_indices));
  bind_scene_vertex_atts(VAOs[1], VBOs[1], EBOs[1], road_vertices,
                         sizeof(road_vertices), road_indices,
                         sizeof(road_indices));



  int modelLoc, viewLoc, projectionLoc, channelLoc;

  // Bind to framebuffer so images displayed there rather than screen
  glBindFramebuffer(GL_FRAMEBUFFER, context->sceneFBO);
  // Makes the sky blue innit
  glClearColor(0.0f, 0.0f, 1.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);

  // Start using the screenShader shaders and link texture
  glUseProgram(context->sceneShader);
  glUniform1i(glGetUniformLocation(context->sceneShader, "ourTexture"), 0);

  glm::mat4 model, view, projection;
	generate_rendering_matrices(&model, &view, &projection, scene);

  // Links up Model, View and Projection matrix with shaders
  modelLoc = glGetUniformLocation(context->sceneShader, "model");
  glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
  viewLoc = glGetUniformLocation(context->sceneShader, "view");
  glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view));
  projectionLoc = glGetUniformLocation(context->sceneShader, "projection");
  glUniformMatrix4fv(projectionLoc, 1, GL_FALSE, glm::value_ptr(projection));
  channelLoc = glGetUniformLocation(context->sceneShader, "channel");

  // Render ground
  // Set channel to 1
  glUniform1i(channelLoc, 1);
  glBindVertexArray(VAOs[0]);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

  // Render road
  // Set channel to 0
  glUniform1i(channelLoc, 0);
  glBindVertexArray(VAOs[1]);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

  // Switch to screen output buffer for safety to stop anything else being
  // rendered to the FBO
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

std::string get_path(std::string &target) {
  auto targetPath = std::filesystem::path(target);
  std::filesystem::path stringCurrentPath = std::filesystem::current_path();
  if (targetPath.is_absolute()) {
    stringCurrentPath += std::filesystem::path(target);
  } else {
    stringCurrentPath /= std::filesystem::path(target);
  }
  return stringCurrentPath.string();
}

GLFWwindow *init_gl_and_get_window() {
  // glfw: initialize and configure
  glfwInit();
#ifdef OGL4
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 6);
#else
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
#endif
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, true);

  // glfw window creation
  GLFWwindow *window =
      glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, "OpenGL", NULL, NULL);
  if (window == NULL) {
    std::cout << "Failed to create GLFW window" << std::endl;
    glfwTerminate();
  }

  glfwMakeContextCurrent(window);

  // Stops textures rendering upsidedown
  stbi_set_flip_vertically_on_load(true);

  // glad: load all OpenGL function pointers
  if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) {
    std::cout << "Failed to initialize GLAD" << std::endl;
  }

  int flags;
  glGetIntegerv(GL_CONTEXT_FLAGS, &flags);
#ifdef OGL4
  if (flags & GL_CONTEXT_FLAG_DEBUG_BIT) {
    glEnable(GL_DEBUG_OUTPUT);
    glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
    glDebugMessageCallback(glDebugOutput, nullptr);
    glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nullptr,
                          GL_TRUE);
  } else {
    std::cout << "We don't have debugging" << std::endl;
  }
#else
  std::cout << "We don't have debugging" << std::endl;
#endif
  return window;
}

void bind_texture(GLuint *texture, std::string &location) {
  glGenTextures(1, texture);
  glBindTexture(GL_TEXTURE_2D, *texture);
  // set the texture wrapping/filtering options (on the currently bound texture
  // object)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  // Load image
  int width, height, nrChannels;
  unsigned char *data =
      stbi_load(location.c_str(), &width, &height, &nrChannels, 0);
  if (data) {
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, width, height, 0, GL_RGB,
                 GL_UNSIGNED_BYTE, data);
    glGenerateMipmap(GL_TEXTURE_2D);
  } else {
    std::cout << "Failed to load texture" << std::endl;
  }
  stbi_image_free(data);
}

void bind_scene_vertex_atts(GLuint VAO, GLuint VBO, GLuint EBO,
                            glm::vec4 *vertices, GLuint vertices_length,
                            GLuint *indices, GLuint indices_length) {
  int total_size = 4 * sizeof(GLfloat); // + sizeof(GLuint);
  glBindVertexArray(VAO);

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, vertices_length, vertices, GL_STATIC_DRAW);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices_length, indices,
               GL_STATIC_DRAW);

  // Position 3D
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, total_size, (void *)0);
  glEnableVertexAttribArray(0);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

void bind_diff_vertex_atts(GLuint VAO, GLuint VBO, GLuint EBO, float *vertices,
                           GLuint vertices_length, GLuint *indices,
                           GLuint indices_length) {
  glBindVertexArray(VAO);

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, vertices_length, vertices, GL_STATIC_DRAW);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices_length, indices,
               GL_STATIC_DRAW);

  // Position 2D
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat),
                        (void *)0);
  glEnableVertexAttribArray(0);

  // Texture coord
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat),
                        (void *)(2 * sizeof(GLfloat)));
  glEnableVertexAttribArray(1);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

void terminate_context() {
  // TODO: Do i need to clean up textures, window as well (I assume so will look
  // into this later)
  glDeleteVertexArrays(1, &context->diffVAO);
  glDeleteBuffers(1, &context->diffVBO);
  glDeleteBuffers(1, &context->diffEBO);
  glDeleteVertexArrays(1, &context->outVAO);
  glDeleteBuffers(1, &context->outVBO);
  glDeleteBuffers(1, &context->outEBO);
  glDeleteBuffers(1, &context->sceneFBO);
  glDeleteBuffers(1, &context->diffFBO);
  glDeleteBuffers(2, context->ssbo_diffs);
  glfwTerminate();
}

#ifdef OGL4
double get_mean_pixel_value(GLuint texture, int color) {
  glFinish();

  glActiveTexture(GL_TEXTURE0);
  glBindImageTexture(0, context->diffTexture, 0, false, 0, GL_READ_ONLY,
                     GL_RGBA32F);

  // std::clock_t    start;
  // start = std::clock();

  glUseProgram(context->computeShader);

  int targetColor = glGetUniformLocation(context->computeShader, "targetColor");
  glm::vec3 targetColors[3];
  targetColors[0] = glm::vec3(56.5 / 255., 56.5 / 255., 56.5 / 255.);
  targetColors[1] = glm::vec3(61.6 / 255., 61.6 / 255., 48.2 / 255.);
  targetColors[2] = glm::vec3(71.8 / 255., 89.8 / 255., 99.2 / 255.0);

  // Render ground
  // Set channel to 1
  glUniform3f(targetColor, targetColors[color][0], targetColors[color][1],
              targetColors[color][2]);

  glBindBuffer(GL_SHADER_STORAGE_BUFFER, context->ssbo_diffs[0]);
  glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, context->ssbo_diffs[0]);

  glBindBuffer(GL_SHADER_STORAGE_BUFFER, context->ssbo_nums[0]);
  glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, context->ssbo_nums[0]);

  // We then run the compute shader
  glDispatchCompute((SCR_WIDTH * SCR_HEIGHT) / (1024 * 2), 1, 1);

  // Make sure all buffers have been loaded
  glFinish();

  int diff_r = *context->ssbo_diff_maps[0];
  int num_r = *context->ssbo_num_maps[0] + 1;

  double normalized_r =
      static_cast<double>(diff_r) / static_cast<double>(num_r);

  // TODO:
  // std::cout << "Time: " << (std::clock() - start) / (double)(CLOCKS_PER_SEC /
  // 1000) << " ms" << std::endl;

  // std::cout << normalized_r / (SCR_WIDTH * SCR_HEIGHT) << std::endl;
  return normalized_r;
}
#else
double get_mean_pixel_value(GLuint texture, int color) {
  // Get average value of the rendered pixels as the value of the deepest mipmap
  // level
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, texture);
  glGenerateMipmap(GL_TEXTURE_2D);

  // Checking the size of the pixel
  int compress_depth = 0;
  int mipmapLevelWidth = -1, mipmapLevelHeight = -1;
  glGetTexLevelParameteriv(GL_TEXTURE_2D, compress_depth, GL_TEXTURE_WIDTH,
                           &mipmapLevelWidth);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, compress_depth, GL_TEXTURE_HEIGHT,
                           &mipmapLevelHeight);
  std::cout << mipmapLevelWidth << std::endl;
  std::cout << mipmapLevelHeight << std::endl;

  // Times by 3 for RGB
  GLfloat *pixels = new GLfloat[mipmapLevelWidth * mipmapLevelHeight * 3];

  glGetTexImage(GL_TEXTURE_2D, 0, GL_RGB, GL_FLOAT, pixels);

  int cumR = 0;
  int cumG = 0;
  int cumB = 0;
  for (int x = 0; x < mipmapLevelWidth; x++) {
    for (int y = 0; y < mipmapLevelHeight; y++) {
      cumR += static_cast<int>(pixels[x * 3 + y * mipmapLevelWidth + 0] * 255);
      cumG += static_cast<int>(pixels[x * 3 + y * mipmapLevelWidth + 1] * 255);
      cumB += static_cast<int>(pixels[x * 3 + y * mipmapLevelWidth + 2] * 255);
    }
  }
  std::cout << (cumR + cumG + cumB) / (mipmapLevelWidth * mipmapLevelHeight)
            << std::endl;
  delete[] pixels;
  return (cumR + cumG + cumB) / (mipmapLevelWidth * mipmapLevelHeight);
}
#endif

void APIENTRY glDebugOutput(GLenum source, GLenum type, GLuint id,
                            GLenum severity, GLsizei length,
                            const char *message, const void *userParam) {
  // ignore non-significant error/warning codes
  if (id == 131169 || id == 131185 || id == 131218 || id == 131204)
    return;

  std::cout << "---------------" << std::endl;
  std::cout << "Debug message (" << id << "): " << message << std::endl;

  switch (source) {
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

  switch (type) {
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

  switch (severity) {
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
