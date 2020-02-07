# Options for selecting the modules to build.
# PCRASTER_BUILD_<module>

# Options for selecting features.
# PCRASTER_WITH_<feature>

# option(PCRASTER_BUILD_ALL
#     "Build everything, except for documentation and tests" FALSE)
# option(PCRASTER_WITH_ALL "Support all features" FALSE)

option(
    PCRASTER_BUILD_MULTICORE
    "Build Python multicore module"
    ON)
CMAKE_DEPENDENT_OPTION(
    PCRASTER_BUILD_TEST_FERN
    "Build Fern unit tests"
    OFF
    "PCRASTER_BUILD_MULTICORE"
    OFF)
option(
    PCRASTER_BUILD_AGUILA
    "Build Aguila"
    ON)
option(
    PCRASTER_BUILD_MODFLOW
    "Build Modflow module"
    ON)
option(
    PCRASTER_BUILD_OLDCALC
    "Build oldcalc"
    ON)
option(
    PCRASTER_BUILD_TEST
    "Build tests"
    OFF)
option(
    PCRASTER_BUILD_DOCUMENTATION
    "Build documentation"
    OFF)
option(
    PCRASTER_BUILD_BLOCKPYTHON
    "Build blockpython module"
    OFF)
option(
    PCRASTER_BUILD_MOC
    "Build moc"
    OFF)
option(
    PCRASTER_BUILD_MLDD
    "Build mldd"
    OFF)
CMAKE_DEPENDENT_OPTION(
    PCRASTER_WITH_OPENGL
    "Use OpenGL for 3D visualisation"
    ON
    "PCRASTER_BUILD_AGUILA"
    OFF)
option(
    PCRASTER_WITH_FLAGS_NATIVE
    "Use architecture flags of host"
    ON)
option(
    PCRASTER_WITH_FLAGS_IPO
    "Use interprocedural optimization"
    OFF)

option(
    PCRASTER_INSTALL_FILES_DEVELOPER
    "Install development files and documentation"
    OFF)
option(
    PCRASTER_INSTALL_FILES_GDAL
    "Install gdal share folder"
    OFF)

# Refactor these at some point? re-check bundle...
option(
    PCRASTER_LINK_STATIC_BOOST
    "Static linking of Boost"
    OFF)
option(
    PCRASTER_PACKAGE_BOOST
    "Package shared libraries"
    OFF)
option(
    PCRASTER_PACKAGE_QT
    "Package shared libraries"
    OFF)
option(
    PCRASTER_PACKAGE_QT_PLATFORMS
    "Package Qt Platform Abstraction directory"
    OFF)
option(
    PCRASTER_PACKAGE_XERCES
    "Package shared libraries"
    OFF)
option(
    PCRASTER_PACKAGE_GDAL
    "Package shared libraries"
    OFF)
option(
    PCRASTER_PACKAGE_NCURSES
    "Package shared libraries"
    OFF)


# Ugly workaround to enforce working wrt conda-build
# TODO: Refactor when all platforms support std filesystem
#       Consider to move to Qt command line options as well
#       to remove (release package) dependency on Boost
if(PCRASTER_LINK_STATIC_BOOST)

    set(DEVBASE_BOOST_REQUIRED TRUE)

    set(Boost_NO_BOOST_CMAKE ON)
    set(Boost_USE_STATIC_LIBS ON)
    set(Boost_USE_STATIC_RUNTIME OFF)

    find_package(Boost COMPONENTS system unit_test_framework date_time filesystem program_options timer)

    if(NOT Boost_FOUND)
        message(FATAL_ERROR "Boost not found")
    else()
        message(STATUS "  includes : ${Boost_INCLUDE_DIRS}")
        message(STATUS "  libraries: ${Boost_LIBRARIES}")
    endif()

    add_definitions(-DBOOST_FILESYSTEM_NO_DEPRECATED)

else()

    set(DEVBASE_BOOST_REQUIRED TRUE)

    set(Boost_NO_BOOST_CMAKE ON)

    if(PCRASTER_BUILD_TEST)
        set(DEVBASE_BUILD_TEST TRUE)
        list(APPEND DEVBASE_REQUIRED_BOOST_COMPONENTS
            system unit_test_framework)

        # The ones below are required in case a developer needs to
        # regenerate one of the pcraster_model_engine's XML files
        # set(DEVBASE_LIB_XSLT_XSLTPROC_REQUIRED TRUE)
        # set(DEVBASE_LIB_XML2_REQUIRED TRUE)
        # set(DEVBASE_LIB_XSLT_REQUIRED TRUE)
        #
        # ... or XSD
        # set(DEVBASE_XSD_REQUIRED TRUE)
    endif()

    list(APPEND DEVBASE_REQUIRED_BOOST_COMPONENTS
        date_time filesystem program_options timer)
endif()

set(DEVBASE_QT_REQUIRED TRUE)
set(DEVBASE_REQUIRED_QT_VERSION 5)
set(DEVBASE_REQUIRED_QT_COMPONENTS
    Core Sql Xml)

if(PCRASTER_BUILD_AGUILA)
    list(APPEND DEVBASE_REQUIRED_QT_COMPONENTS Gui Widgets Charts)
endif()

if(PCRASTER_WITH_OPENGL)
    find_package(OpenGL REQUIRED)
    list(APPEND DEVBASE_REQUIRED_QT_COMPONENTS OpenGL)
endif()


find_package(XercesC REQUIRED)

set(DEVBASE_GDAL_REQUIRED TRUE)  # Version >= 2.0.0.



find_package(Python3 COMPONENTS Interpreter Development NumPy)
message(STATUS "  Interpreter ID: " ${Python3_INTERPRETER_ID})

if(UNIX)
    set(CURSES_NEED_NCURSES TRUE)
    set(CURSES_NEED_WIDE TRUE)
    find_package(Curses REQUIRED)
    message(STATUS "Found ncurses: ")
    message(STATUS "  includes : " ${CURSES_INCLUDE_DIRS})
    message(STATUS "  libraries: " ${CURSES_LIBRARIES})
endif()

if(PCRASTER_BUILD_TEST)

    find_python_module(psutil)

    # sqlite executable is used by dal's testrun.prolog
    find_package(SQLite3)

    find_program(SQLITE3_EXECUTABLE
        sqlite3
        # HINTS ${SQLITE3_INCLUDE_DIRS}/../bin
        HINTS $<$<BOOL:${SQLITE_FOUND}>:${SQLITE3_INCLUDE_DIRS}/../bin>
    )

    if(NOT SQLITE3_EXECUTABLE)
        message(FATAL_ERROR "sqlite3 executable not found")
    endif()
endif()


if(PCRASTER_BUILD_DOCUMENTATION)
    set(DEVBASE_DOXYGEN_REQUIRED TRUE)
    set(DEVBASE_SPHINX_REQUIRED TRUE)
    set(SPHINX_HTML_THEME "classic")
endif()

# pybind11
# C++ version flags should match ours
if(NOT CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
    set(PYBIND11_CPP_STANDARD -std=c++17)
else()
    set(PYBIND11_CPP_STANDARD /std:c++17)
endif()

# This variable may be set from somewhere (on Windows) leading to
# Python version mixups. Enforce the desired one:
set(PYTHON_EXECUTABLE ${Python3_EXECUTABLE})

# Python.h needs to be known to pass the test
set (CMAKE_REQUIRED_INCLUDES "${Python3_INCLUDE_DIRS};${CMAKE_REQUIRED_INCLUDES}")

check_include_file_cxx("pybind11/pybind11.h" PYBIND11_SYSTEM_INCLUDE)

if(NOT PYBIND11_SYSTEM_INCLUDE)
    FetchContent_Declare(
        pybind11
        GIT_REPOSITORY https://github.com/pybind/pybind11
        GIT_TAG        v2.4.3
    )

    FetchContent_GetProperties(pybind11)
    if(NOT pybind11_POPULATED)
        FetchContent_Populate(pybind11)
        add_subdirectory(${pybind11_SOURCE_DIR} ${pybind11_BINARY_DIR})
    endif()
else()
    find_package(pybind11 REQUIRED)
endif()
