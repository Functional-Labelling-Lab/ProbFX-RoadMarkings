#include "bindings.h"
#include "main.h"
#include "hough.h"
extern opengl_context* context;

void check_context(){
	if (!context) {	
		init_context();	
	}	
}

void render_scene_c(struct scene *scene, GLuint FBO) {
	check_context();
	render_scene(scene, FBO);
}
void set_target_img_c(const char *str) {
	check_context();
	set_target_img(str);
} 
int test_bed_c(double x, double y, double z, double pitch, double yaw, double roll) {
	check_context();
	return test_bed(x,y,z,pitch,yaw,roll);
}
double get_mean_pixel_value_c() {
	check_context();
	return get_mean_pixel_value(context->diffTexture);
}
void find_texture_difference_c() {
	check_context();
	find_texture_difference(context->sceneTexture,context->targetTexture);
}

// Hough transform bindings
detected_lines_t *hough_lines_c(const char *str) {
	check_context();
	return detect_lines(str);
}

struct texture_fbo *create_texture_fbo_c() {
	check_context();
	return create_texture_fbo();
}

GLuint get_scene_fbo_c() {
	check_context();
	return get_scene_fbo();
}

GLuint get_target_texture_c() {
	check_context();
	return get_target_texture();
}

