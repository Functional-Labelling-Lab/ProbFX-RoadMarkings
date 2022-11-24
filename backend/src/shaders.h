#ifndef SHADERS_H
#define SHADERS_H

#include <GLFW/glfw3.h>
#include <glad/glad.h>

#include <string>

#ifdef SHADER_CONSTS
const char *scene_vertex =
#include "shaders/scene_vert.glsl"
    ;

const char *scene_fragment =
#include "shaders/scene_frag.glsl"
    ;

const char *diff_vertex =
#include "shaders/diff_vert.glsl"
    ;

const char *diff_fragment =
#include "shaders/diff_frag.glsl"
    ;

const char *out_vertex =
#include "shaders/out_vert.glsl"
    ;

const char *out_fragment =
#include "shaders/out_frag.glsl"
    ;
#endif

GLuint load_shader(const char *vertexCode, const char *fragmentCode);

#endif