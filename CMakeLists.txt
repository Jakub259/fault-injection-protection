cmake_minimum_required(VERSION 3.20.0)


project(Cfip)

# https://llvm.org/docs/CMake.html#developing-llvm-passes-out-of-source
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

if(NOT "${LLVM_VERSION_MAJOR}" MATCHES "^19.*")
  message(FATAL_ERROR "Found LLVM ${LLVM_VERSION_MAJOR}, but LLVM 19 is required")
endif()

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})


# Set the build directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")

add_subdirectory(llvm_plugin)
add_subdirectory(firv2/llvm-firv2)

