.PHONY: configure clean build

all: configure build

configure:
	mkdir -p build
	cmake -S . -B build

build:
	make -C build
	rm -rf libs && mkdir libs
	
	# copy library files over - TODO: merge with AR
	cp build/libRoadMarkingsBackend.a libs/libRoadMarkingsBackend.a
	cp build/_deps/glfw-build/src/libglfw3.a libs/libglfw3.a
	cp build/_deps/glad-build/libglad.a libs/libglad.a
clean:
	rm -rf build
	rm -rf libs
