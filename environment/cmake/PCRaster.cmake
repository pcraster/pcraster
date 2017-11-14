set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN 1)

include(PeacockPlatform)
include(DevBaseCompiler)

set(CMAKE_CXX_FLAGS_DEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} -DDEBUG -DDEBUG_BUILD -DDEBUG_DEVELOP"
)
set(CMAKE_C_FLAGS_DEBUG
    "${CMAKE_C_FLAGS_DEBUG} -DDEBUG -DDEBUG_BUILD -DDEBUG_DEVELOP"
)
# https://svn.boost.org/trac/boost/ticket/6455
set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} -DQT_NO_KEYWORDS"
)


include(PCRasterConfiguration)
include(DevBaseExternal)

if(DEVBASE_SQLITE_EXECUTABLE_REQUIRED)
    find_program(SQLITE3_EXECUTABLE
        sqlite3
        HINTS ${SQLITE3_INCLUDE_DIRS}/../bin
    )
    if(NOT SQLITE3_EXECUTABLE)
        message(FATAL_ERROR "sqlite3 executable not found")
    endif()
endif()

include(DevBaseMacro)
include(PCRasterMacro)

if(PCRASTER_BUILD_TEST)
    enable_testing()
endif()

set(PCRASTER_DATA_DIR ${PROJECT_SOURCE_DIR}/data)


force_out_of_tree_build()


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
