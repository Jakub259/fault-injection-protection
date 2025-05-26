list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)

set(CMAKE_CXX_STANDARD 20)

add_compile_options(-Wall -Werror -Wconversion -Wsign-conversion 
                    -Wpedantic -Wvla)

add_llvm_pass_plugin(LLVMCfip MODULE
  cfip.cpp
  )
