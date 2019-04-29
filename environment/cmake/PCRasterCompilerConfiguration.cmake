set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN 1)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)


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


# use toolchain files to select compiler flags...




# TODO Get rid of these...
set(CMAKE_CXX_FLAGS_DEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} -DDEBUG -DDEBUG_BUILD -DDEBUG_DEVELOP"
)
set(CMAKE_C_FLAGS_DEBUG
    "${CMAKE_C_FLAGS_DEBUG} -DDEBUG -DDEBUG_BUILD -DDEBUG_DEVELOP"
)

