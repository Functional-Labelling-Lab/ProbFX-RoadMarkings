#ifndef BINDINGS_H
#define BINDINGS_H

#include <glad/glad.h>
#include "hough.h"

extern "C" {
void render_scene_c(struct scene *scene);
void set_target_img_c(const char *str) ;
int test_bed_c(double x, double y, double z, double pitch, double yaw, double roll);
double get_mean_pixel_value_c();
void find_texture_difference_c();
detected_lines_t *hough_lines_c(const char *str);
GLuint create_texture_fbo_c();
}

#endif
