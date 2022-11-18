#include "bindings.h"
#include "main.h"
#include "hough.h"
extern opengl_context* context;

void check_context(){
	if (!context) {	
		init_context();	
	}	
}

void render_scene_c(struct scene *scene) {
	check_context();
	render_scene(scene);
}
void set_target_img_c(const char *str) {
	check_context();
	set_target_img(str);
} 
int test_bed_c(double x, double y, double z, double pitch, double yaw, double roll) {
	check_context();
	return test_bed(x,y,z,pitch,yaw,roll);
}
double get_mean_pixel_value_c(int color) {
	check_context();
	return get_mean_pixel_value(context->diffTexture, color);
}
void find_texture_difference_c(int color) {
	check_context();
	get_image_mask(context->sceneTexture,context->targetTexture,color);
}

// Hough transform bindings
detected_lines_t *hough_lines_c(const char *str) {
	check_context();
	return detect_lines(str);
}
