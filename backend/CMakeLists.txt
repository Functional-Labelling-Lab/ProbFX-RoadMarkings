cmake_minimum_required(VERSION 3.16)

include(FetchContent)

project(RoadMarkingsBackend)

# Use openGL 4 if true, 3 is false
set(OGL4 true)

if (${OGL4})
    message(STATUS "Building for OpenGL 4")
else()
    message(STATUS "Building for OpenGL 3")
endif()
message(STATUS "To change OpenGL version, change the OGL4 variable in backend/CMakeLists.txt")

add_library(${PROJECT_NAME} STATIC src/main.cpp src/shaders.cpp src/bindings.cpp src/hough.cpp)
# add_executable(${PROJECT_NAME} src/main.cpp src/shaders.cpp src/bindings.cpp src/hough.cpp)

# OpenGL is required, including for glfw
find_package( OpenGL REQUIRED )
find_package( OpenCV REQUIRED )

include_directories( ${OPENGL_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR})

# Get glfw
FetchContent_Declare(
    glfw
    GIT_REPOSITORY https://github.com/glfw/glfw
)

FetchContent_GetProperties(glfw)
if(NOT glfw_POPULATED)
    FetchContent_Populate(glfw)

    set(GLFW_BUILD_EXAMPLES OFF CACHE INTERNAL "Build the GLFW example programs")
    set(GLFW_BUILD_TESTS OFF CACHE INTERNAL "Build the GLFW test programs")
    set(GLFW_BUILD_DOCS OFF CACHE INTERNAL "Build the GLFW documentation")
    set(GLFW_INSTALL OFF CACHE INTERNAL "Generate installation target")

    add_subdirectory(${glfw_SOURCE_DIR} ${glfw_BINARY_DIR})
endif()

FetchContent_Declare(
    glad
    GIT_REPOSITORY https://github.com/Dav1dde/glad.git
)

FetchContent_GetProperties(glad)
if(NOT glad_POPULATED)
    FetchContent_Populate(glad)
    set(GLAD_PROFILE "core" CACHE STRING "OpenGL profile")
    if (${OGL4})
        set(GLAD_API "gl=4.6" CACHE STRING "API type/version pairs, like \"gl=3.2,gles=\", no version means latest")
    else()
        set(GLAD_API "gl=" CACHE STRING "API type/version pairs, like \"gl=3.2,gles=\", no version means latest")
    endif()
    set(GLAD_GENERATOR "c" CACHE STRING "Language to generate the binding for")
    add_subdirectory(${glad_SOURCE_DIR} ${glad_BINARY_DIR})
endif()

FetchContent_Declare(
        glm
        GIT_REPOSITORY https://github.com/g-truc/glm
)
FetchContent_GetProperties(glm)
if(NOT glm_POPULATED)
    FetchContent_Populate(glm)
	# set(GLM_TEST_ENABLE OFF CACHE BOOL "" FORCE)
    add_subdirectory(${glm_SOURCE_DIR} ${glm_BINARY_DIR})
endif()

FetchContent_Declare(
	stb_image
	GIT_REPOSITORY https://github.com/jschmidt42/stb-cmake.git
)
FetchContent_GetProperties(stb_image)
if(NOT stb_image_POPULATED)
  FetchContent_Populate(stb_image)
endif()

include_directories(${stb_image_SOURCE_DIR} ${OpenCV_INCLUDE_DIRS})

if (${OGL4})
    add_definitions(-DOGL4)
endif()

# C++ compiler flags
set(CMAKE_CXX_FLAGS_RELEASE "-O3")
set(CMAKE_CXX_FLAGS "-lstdc++fs -std=c++17 -fPIC")

target_link_libraries(${PROJECT_NAME} ${OpenCV_LIBS} GL dl glfw glad glm m stdc++fs)
