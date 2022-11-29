## RoadMarkings Backend
### What is this
This contains the C++ side of the codebase.
- Rendering & road geometry
- Fast image comparison (MSE)
- openCV functionality

The makefile contained is called by cabal when building the Haskell project to get the static libraries for glfw3, glad and the roadmarkings backend, these are then linked with haskell.

### File Structure
```Bash
├── CMakeLists.txt  # CMake dependency & build management
├── Makefile        # Makefile used to configure, run cmake & make libraries
├── README.md       # This readme
└── src  # Contains all C++ source code
    ├── bindings.cpp/h
    ├── render.cpp/h    # rendering (synthetic, and for display)
    ├── shaders.cpp/h   # shader utilities and includes from shaders/
    ├── shaders ─── ... # glsl shaders source
    └── images ──── ... # images used for textures
```

### Develop
```Bash
make            # Configure and build!
make configure  # Configures cmake source and out directories
make build      # Builds libraries with make, and places them in libs
make clean      # Removes build and libs
```
