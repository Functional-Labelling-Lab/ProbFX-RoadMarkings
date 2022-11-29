![probFX-RoadMarkings](https://user-images.githubusercontent.com/44177991/196529156-eb616529-baff-4b82-b078-9d083787d3cf.png)

## What is this?
This repository contains our attempt at developing a working model for auto labelling road marking geometry on images.
- Indending to use ProbFX (or a derivative) to allow for ergonomic & reusable models
- Currently using the [Haskell Image Processing Library (HIP)](https://github.com/lehins/hip) for image handling (currently unstable, so looking to move to an [openCV](https://opencv.org/) binding)

## How will this work?
The long-term goal is to build the pipeline described below...

![Pipeline drawio](https://user-images.githubusercontent.com/44177991/196551636-ffc268d2-25db-4bad-8f2f-d19663754dfd.png)


## File Structure
```bash
├── CHANGELOG.md
├── README.md     # This readme
|
├── ProbFX-RoadMarkings.cabal  # Cabal configuration
├── Setup.hs                   # Hooks for running C++ side build
├── cabal.project              # ProbFX dependency
|
├── data ───── ... # Data used for testing (i.e images)
|
├── .vscode
|    └── settings.json  # Helpful vscode configuration (e.g for clang tidy extension)
|
├── backend ── ... # The C++ side of the project
├── app ────── ... # The main 
└── src ────── ... # The Haskell side of the project (contains bindings, logic)
```

## Repo Goals (Short Term)
- Implement basic image transformations (hough, edge detections, filtering etc)
- Implement a basic model for road geometry
- Improve basic equation to error function check routines
- Implement our own metropolis hastings to improve/refine model parameters on a single image


## Building The Project
### System Dependencies
On Ubuntu ensure you have installed the following:
```bash
sudo apt-get install 
    # required for C++ build
    make
    cmake
    g++ 

    # Required for openCV install
    wget 
    unzip 

    # Required for OpenGL
    libx11-dev 
    mesa-common-dev 
    libgl1-mesa-dev 
    libglu1-mesa-dev 
    libxinerama1 
    libxinerama-dev 
    libxcursor-dev 
    libxrandr-dev 
    libxi-dev 
    libxmu-dev 
    libblas-dev
```
OpenCV is also required for this project ande can be installed with the [following instructions](https://docs.opencv.org/4.x/d7/d9f/tutorial_linux_install.html#tutorial_linux_install_detailed_basic_download):
```bash
# create a directory to build openCV in:
mkdir openCV && cd openCV

# Download the required sources:
wget -O opencv.zip https://github.com/opencv/opencv/archive/4.x.zip
unzip opencv.zip
mv opencv-4.x opencv

# create build directory
mkdir -p build && cd build

# Configure cmake and build
cmake ../opencv
make -j4

# install system-wide
sudo make install
```
To complete installation a reboot is required, otherwise you may get a `cannot open shared object file` error ([similar issue](https://stackoverflow.com/questions/12335848/opencv-program-compile-error-libopencv-core-so-2-4-cannot-open-shared-object-f)). On WSL use:
```powershell
wsl --shutdown
# reopen wsl terminal to restart
```

### Build & Clean
To simplify the build process we use a makefile to invoke cabal
```bash
make       # same as make build
make run
make build
make clean
```
