set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN 1)
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# On Linux/macOS we set the RPATH for our libraries/executables
# From the CMake wiki:

# use, i.e. don't skip the full RPATH for the build tree
set(CMAKE_SKIP_BUILD_RPATH  OFF)

# when building, don't use the install RPATH already
# (but later on when installing)
set(CMAKE_BUILD_WITH_INSTALL_RPATH OFF)

# the RPATH to be used when installing
set(CMAKE_INSTALL_RPATH "$ORIGIN/../lib")

# don't add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Setting postfix is disabled in several Python modules
# and linkin related libraries
set(CMAKE_DEBUG_POSTFIX "_d")

# When not specified: default build type set to Release
if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# Only allow supported build configurations
set(SUPPORTED_BUILD_CONFIGURATIONS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")

if(NOT ${CMAKE_BUILD_TYPE} IN_LIST SUPPORTED_BUILD_CONFIGURATIONS)
    message(FATAL_ERROR "Unsupported build type '" ${CMAKE_BUILD_TYPE} "', use: Debug Release MinSizeRel RelWithDebInfo")
endif()

foreach(BUILD_TYPE ${CMAKE_CONFIGURATION_TYPES})
    if(NOT ${BUILD_TYPE} IN_LIST SUPPORTED_BUILD_CONFIGURATIONS)
        message(FATAL_ERROR "Unsupported build type '" ${BUILD_TYPE} "', use: Debug Release MinSizeRel RelWithDebInfo")
    endif()
endforeach()

if(PCRASTER_WITH_FLAGS_NATIVE)
    add_compile_options(
        "$<$<CONFIG:Release>:$<$<COMPILE_LANG_AND_ID:C,GNU,AppleClang,Clang>:-march=native;-mtune=native>>"
        "$<$<CONFIG:Release>:$<$<COMPILE_LANG_AND_ID:CXX,GNU,AppleClang,Clang>:-march=native;-mtune=native>>"
        "$<$<CONFIG:RelWithDebInfo>:$<$<COMPILE_LANG_AND_ID:C,GNU,AppleClang,Clang>:-march=native;-mtune=native>>"
        "$<$<CONFIG:RelWithDebInfo>:$<$<COMPILE_LANG_AND_ID:CXX,GNU,AppleClang,Clang>:-march=native;-mtune=native>>"
    )
endif()

add_compile_options(
    "$<$<COMPILE_LANG_AND_ID:C,GNU,AppleClang,Clang>:-pipe;-Werror=missing-include-dirs>"
    "$<$<COMPILE_LANG_AND_ID:CXX,GNU,AppleClang,Clang>:-pipe;-Werror=missing-include-dirs>"
    "$<$<COMPILE_LANG_AND_ID:C,AppleClang,Clang>:>"
    "$<$<COMPILE_LANG_AND_ID:CXX,AppleClang,Clang>:>"
    "$<$<COMPILE_LANG_AND_ID:C,GNU>:-pedantic;>"
    "$<$<COMPILE_LANG_AND_ID:CXX,GNU>:-pedantic;>"
    "$<$<CXX_COMPILER_ID:MSVC>:/W1>"
    #"$<$<AND:$<PLATFORM_ID:Linux>,$<COMPILE_LANG_AND_ID:C,GNU>,$<CONFIG:Debug>>:-Wall;-pedantic;-Wpointer-arith;-Wdeclaration-after-statement>"
    #"$<$<AND:$<PLATFORM_ID:Linux>,$<COMPILE_LANG_AND_ID:CXX,GNU>,$<CONFIG:Debug>>:>"
    #"$<$<AND:$<PLATFORM_ID:Linux>,$<COMPILE_LANG_AND_ID:C,GNU>,$<CONFIG:Release>>:-Wall;-pedantic;-Wpointer-arith;-Wdeclaration-after-statement;-Wno-maybe-uninitialized;-Wstrict-prototypes>"
    #"$<$<AND:$<PLATFORM_ID:Linux>,$<COMPILE_LANG_AND_ID:C,AppleClang,Clang>,$<CONFIG:Release>>:-Wall;-pedantic;-Wno-enum-conversion;-Wno-tautological-compare>"
    #"$<$<AND:$<PLATFORM_ID:Linux>,$<COMPILE_LANG_AND_ID:CXX,GNU>,$<CONFIG:Release>>:>"
    #"$<$<AND:$<PLATFORM_ID:Linux>,$<CXX_COMPILER_ID:Clang>>:-stdlib=libc++>" #;-lc++abi  -stdlib=libc++;-D_GLIBCXX_USE_CXX11_ABI=0
)

add_compile_definitions(
  "$<$<AND:$<COMPILE_LANGUAGE:C,CXX>,$<CONFIG:Debug>>:DEBUG;DEBUG_BUILD;DEBUG_DEVELOP>"
  #
  "$<$<COMPILE_LANG_AND_ID:C,MSVC>:_CRT_SECURE_NO_WARNINGS;_USE_MATH_DEFINES;NOMINMAX;_SILENCE_STDEXT_ARR_ITERS_DEPRECATION_WARNING>"
  "$<$<COMPILE_LANG_AND_ID:CXX,MSVC>:_CRT_SECURE_NO_WARNINGS;NOMINMAX;_SILENCE_STDEXT_ARR_ITERS_DEPRECATION_WARNING>"
  #"$<$<OR:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>:_CRT_SECURE_NO_WARNINGS;_USE_MATH_DEFINES;NOMINMAX>"
)

# Based on conda and https://developers.redhat.com/blog/2018/03/21/compiler-and-linker-flags-gcc/
# https://wiki.ubuntu.com/ToolChain/CompilerFlags?action=show&redirect=CompilerFlags
# https://wiki.debian.org/Hardening
add_link_options(
    "$<$<OR:$<CXX_COMPILER_ID:GNU>>:-Wl,--no-undefined;-Wl,--as-needed;-Wl,--sort-common;-Wl,--gc-sections;-Wl,-O2;-Wl,-z,now;-Wl,-z,relro;-Wl,-z,defs;-Wl,--warn-common;-Wl,--hash-style=gnu;-Wl,--no-copy-dt-needed-entries>"
    "$<$<AND:$<PLATFORM_ID:Linux>,$<CXX_COMPILER_ID:Clang>>:-Wl,--as-needed;-Wl,--sort-common;-Wl,--gc-sections;-Wl,-O2;-Wl,-z,now;-Wl,-z,relro;-Wl,--warn-common;-Wl,--hash-style=gnu;-Wl,--no-copy-dt-needed-entries>"

)

if(PCRASTER_WITH_FLAGS_IPO)
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


# if(MSVC)
#     # TODO add debug/release flags?
#
#     # Get rid of the min and max macros.
#     # Refactor the define private/protected public stuff (allow keywords macro)
#     add_compile_definitions(
#         -D_SCL_SECURE_NO_WARNINGS
#         -D_CRT_SECURE_NO_WARNINGS
#         -D_USE_MATH_DEFINES
#         -DNOMINMAX
#         -D_ALLOW_KEYWORD_MACROS
#     )
#
#
#     # add /w3
#     # disable these warnings?
#     set(CMAKE_CXX_FLAGS
#         "${CMAKE_CXX_FLAGS} /std:c++14 /wd4244 /wd4396 /wd4305"
#     )
#
# endif()
