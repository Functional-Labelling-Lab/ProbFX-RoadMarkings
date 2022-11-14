#include "bindings.h"
#include "main.h"

extern opengl_context* context;

void render_scene_c(struct scene *scene) {
	render_scene(scene);
}
void set_target_img_c(const char *str) {
	set_target_img(str);
} 
int test_bed_c(double x, double y, double z, double pitch, double yaw, double roll, double roadWidth) {
	return test_bed(x,y,z,pitch,yaw,roll,roadWidth);
}
double get_mean_pixel_value_c() {
	if (context == NULL) {
		//Throw Error
		return -1;
	}
	return get_mean_pixel_value(context->diffTexture);
}
void find_texture_difference_c() {
	if (context == NULL) {
		//Throw Error
	} else {
		find_texture_difference(context->sceneTexture,context->targetTexture);
	}

} 