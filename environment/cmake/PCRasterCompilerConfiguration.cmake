set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN 1)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# On Linux/macOS we set the RPATH for our libraries/executables
# From the CMake wiki:

# use, i.e. don't skip the full RPATH for the build tree
set(CMAKE_SKIP_BUILD_RPATH  OFF)

# when building, don't use the install RPATH already
# (but later on when installing)
set(CMAKE_BUILD_WITH_INSTALL_RPATH OFF)

# the RPATH to be used when installing
set(CMAKE_INSTALL_RPATH "\\\$ORIGIN/../lib")

# don't add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH OFF)


# Default flags without toolchain files
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

if(${CMAKE_BUILD_TYPE} STREQUAL "Release")
    add_compile_options(
        "$<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>:-march=native;-mtune=native>"
    )
endif()

add_compile_options(
    "$<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>:-pipe>"
)

# Based on conda and https://developers.redhat.com/blog/2018/03/21/compiler-and-linker-flags-gcc/
# Check https://wiki.ubuntu.com/ToolChain/CompilerFlags?action=show&redirect=CompilerFlags
add_link_options(
    "$<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>:-Wl,--no-undefined;-Wl,--as-needed;-Wl,--sort-common;-Wl,--gc-sections;-Wl,-O2;-Wl,-z,now;-Wl,-z,relro;-Wl,-z,defs>"
)

if(PCRASTER_WITH_IPO)
    include(CheckIPOSupported)
    check_ipo_supported(RESULT COMPILER_HAS_IPO)
    # This takes long for the unit tests...
    if(COMPILER_HAS_IPO)
        set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
    endif()
endif()



# In general, don't set compiler options in CMake files. Here we set the
# most general options that everybody always wants. Anything else should
# be handled from the outside.
# Not yet (treat warnings as errors):
#   MSVC: /WX
#   GNU/Clang: -Werror
# add_compile_options(
#     "$<$<CXX_COMPILER_ID:MSVC>:/W3 /WX>"
#     "$<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>:-W -Wall>"
# )






# TODO Get rid of these...
set(CMAKE_CXX_FLAGS_DEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} -DDEBUG -DDEBUG_BUILD -DDEBUG_DEVELOP"
)
set(CMAKE_C_FLAGS_DEBUG
    "${CMAKE_C_FLAGS_DEBUG} -DDEBUG -DDEBUG_BUILD -DDEBUG_DEVELOP"
)

