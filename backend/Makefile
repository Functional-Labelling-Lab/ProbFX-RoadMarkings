.PHONY: configure clean build

all: configure build

configure:
	mkdir -p build
	cmake -S . -B build

build:
	make -C build

clean:
	rm -rf build
