# https://github.com/geoneric/peacock/blob/master/cmake/PeacockPlatform.cmake
include(PeacockPlatform) # This one first. Other modules use the variables.

if(WIN32)
    set(CMAKE_DEBUG_POSTFIX "d")
endif()


# Get rid of these after refactoring. ------------------------------------------
ADD_CUSTOM_TARGET(tests)

SET(LIBRARY_OUTPUT_PATH
    ${PROJECT_BINARY_DIR}/bin
    CACHE PATH
    "Single directory for all libraries."
)
set(EXECUTABLE_OUTPUT_PATH
    ${PROJECT_BINARY_DIR}/bin
    CACHE PATH
    "Single directory for all executables."
)
MARK_AS_ADVANCED(
    # No need to see these, just do it.
    LIBRARY_OUTPUT_PATH
    EXECUTABLE_OUTPUT_PATH
)
# /Get rid of these after refactoring. -----------------------------------------






include(PCRasterCompiler)  # This one first. Configuration uses the compiler.
include(PCRasterConfiguration)
include(PCRasterExternal)
include(PCRasterMacro)

if(PCRASTER_BUILD_TEST)
    enable_testing()
endif()

force_out_of_tree_build()

set(PCRASTER_DATA_DIR ${PROJECT_SOURCE_DIR}/data)

# TODO Print status information about the current build.
