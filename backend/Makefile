.PHONY: configure clean build

build_dir = build
lib_dir = lib

bold_magenta = \033[1;33m
reset_col = \033[0m

all: configure build

configure:
	@printf "$(bold_magenta)>> Configuring CMake       <<$(reset_col)\n"
	mkdir -p $(build_dir)
	cmake -S . -B $(build_dir)
	@printf "$(bold_magenta)>> Cmake Configure Complete<<$(reset_col)\n"

build:
	@printf "$(bold_magenta)>> Build with makefile     <<$(reset_col)\n"
	make -C $(build_dir)
	
	@printf "$(bold_magenta)>> Copy libs into libs     <<$(reset_col)\n"
	rm -rf $(lib_dir) && mkdir $(lib_dir)
	cp $(build_dir)/libRoadMarkingsBackend.a        $(lib_dir)/libRoadMarkingsBackend.a
	cp $(build_dir)/_deps/glfw-build/src/libglfw3.a $(lib_dir)/libglfw3.a
	cp $(build_dir)/_deps/glad-build/libglad.a      $(lib_dir)/libglad.a
	@printf "$(bold_magenta)>> CPP Build Complete      <<$(reset_col)\n"

clean:
	@printf "$(bold_magenta)>> Cleaning CPP Side       <<$(reset_col)\n"
	rm -rf $(build_dir)
	rm -rf $(lib_dir)
	@printf "$(bold_magenta)>> CPP Side clean Complete <<$(reset_col)\n"
