#ifndef BINDINGS_H
#define BINDINGS_H

#include <glad/glad.h>
#include "hough.h"
#include "main.h"

extern "C" {
void render_scene_c(struct scene *scene, GLuint FBO);
void set_target_img_c(const char *str);
int test_bed_c(double x, double y, double z, double pitch, double yaw,
               double roll);
double get_mean_pixel_value_c(int color);
void find_texture_difference_c(int color);
detected_lines_t *hough_lines_c(const char *str);

struct texture_fbo *create_texture_fbo_c();
GLuint get_scene_fbo_c();
GLuint get_target_texture_c();
}

#endif
