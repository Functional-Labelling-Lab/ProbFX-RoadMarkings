#ifndef SHADERS_H
#define SHADERS_H

#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include <string>



void check_compile_errors(GLuint shader, std::string type);
GLuint load_shader(std::string vertexPath, std::string fragmentPath);

#endif