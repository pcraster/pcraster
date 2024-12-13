include(GenerateExportHeader)
include(CheckIncludeFileCXX)
include(FetchContent)
include(CMakeDependentOption)
include(CPM)

include(PCRasterMacro)
include(PCRasterConfiguration)
include(PCRasterCompilerConfiguration)
include(PCRasterTestMacro)

# Get required dependencies first...
CPMAddPackage("gh:pcraster/rasterformat#fffaf990a2cd6365c4122ced5883f74bbc55e4f0")


set(PCRASTER_DATA_DIR ${PROJECT_SOURCE_DIR}/data)


# Get rid of these after refactoring. ------------------------------------------
add_custom_target(tests)

set(LIBRARY_OUTPUT_PATH
    ${PROJECT_BINARY_DIR}/bin
    CACHE PATH
    "Single directory for all libraries."
)
set(EXECUTABLE_OUTPUT_PATH
    ${PROJECT_BINARY_DIR}/bin
    CACHE PATH
    "Single directory for all executables."
)
mark_as_advanced(
    # No need to see these, just do it.
    LIBRARY_OUTPUT_PATH
    EXECUTABLE_OUTPUT_PATH
)
# /Get rid of these after refactoring. -----------------------------------------
