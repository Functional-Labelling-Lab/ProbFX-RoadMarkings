.PHONY: configure clean build

build_dir = build
lib_dir = lib

all: configure build

configure:
	mkdir -p $(build_dir)
	cmake -S . -B $(build_dir)

build:
	make -C $(build_dir)
	rm -rf $(lib_dir) && mkdir $(lib_dir)
	
	# copy library files over - TODO: merge with AR
	cp $(build_dir)/libRoadMarkingsBackend.a        $(lib_dir)/libRoadMarkingsBackend.a
	cp $(build_dir)/_deps/glfw-build/src/libglfw3.a $(lib_dir)/libglfw3.a
	cp $(build_dir)/_deps/glad-build/libglad.a      $(lib_dir)/libglad.a
clean:
	rm -rf $(build_dir)
	rm -rf $(lib_dir)
