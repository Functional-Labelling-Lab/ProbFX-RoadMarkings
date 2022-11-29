#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include <iostream>

#include "render.h"
#include "shaders.h"

enum ShaderType { VERTEX, PROGRAM, FRAGMENT };

void check_compile_errors(GLuint shader, enum ShaderType type) {
  GLint success;
  GLchar infoLog[1024];
  if (type != PROGRAM) {
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    if (!success) {
      glGetShaderInfoLog(shader, 1024, NULL, infoLog);
      std::cout << "ERROR::SHADER_COMPILATION_ERROR of type: " << type
                << std::endl
                << infoLog << std::endl;
    }
  } else {
    glGetProgramiv(shader, GL_LINK_STATUS, &success);
    if (!success) {
      glGetProgramInfoLog(shader, 1024, NULL, infoLog);
      std::cout << "ERROR::PROGRAM_LINKING_ERROR of type: " << type << std::endl
                << infoLog << std::endl;
    }
  }
}

GLuint load_shader(const char *vShaderCode, const char *fShaderCode) {
  unsigned int vertex = glCreateShader(GL_VERTEX_SHADER);
  glShaderSource(vertex, 1, &vShaderCode, NULL);
  glCompileShader(vertex);
  check_compile_errors(vertex, VERTEX);

  unsigned int fragment = glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(fragment, 1, &fShaderCode, NULL);
  glCompileShader(fragment);
  check_compile_errors(fragment, FRAGMENT);

  GLuint id = glCreateProgram();
  glAttachShader(id, vertex);
  glAttachShader(id, fragment);
  glLinkProgram(id);
  check_compile_errors(id, PROGRAM);

  // Delete shaders - linked & no longer necessary
  glDeleteShader(vertex);
  glDeleteShader(fragment);

  return id;
}
