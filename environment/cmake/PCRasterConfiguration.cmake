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
option(
    PCRASTER_BUILD_AGUILA
    "Build Aguila"
    ON)
option(
    PCRASTER_BUILD_MODFLOW
    "Build Modflow module"
    ON)
CMAKE_DEPENDENT_OPTION(
    PCRASTER_BUILD_MODFLOW_EXECUTABLE
    "Build Modflow 2005 executable"
    OFF
    "PCRASTER_BUILD_MODFLOW"
    OFF)
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
    ON)
option(
    PCRASTER_BUILD_MOC
    "Build moc"
    ON)
option(
    PCRASTER_BUILD_MLDD
    "Build mldd"
    ON)
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
option(
    PCRASTER_WITH_QT6
    "Use Qt6"
    OFF)

if(NOT PCRASTER_PYTHON_INSTALL_DIR)
    set(PCRASTER_PYTHON_INSTALL_DIR python)
endif()

if(NOT PCRASTER_BIN_INSTALL_DIR)
  set(PCRASTER_BIN_INSTALL_DIR bin)
endif()

if(NOT PCRASTER_LIB_INSTALL_DIR)
  set(PCRASTER_LIB_INSTALL_DIR lib)
endif()


set(Boost_NO_BOOST_CMAKE ON)
# No more linking to Boost required for released components
# all header-only  >=1.73
# keep date_time for current CI
list(APPEND PCR_BOOST_COMPONENTS date_time)

if(PCRASTER_BUILD_TEST)
    enable_testing()
    list(APPEND PCR_BOOST_COMPONENTS unit_test_framework)
endif()

# >=1.73 required for header-only date_time
find_package(Boost 1.71 REQUIRED COMPONENTS ${PCR_BOOST_COMPONENTS})


if(PCRASTER_BUILD_MULTICORE)
    CPMAddPackage(
        GITHUB_REPOSITORY geoneric/fern
        GIT_TAG cfb1c584aa8b3299cb59506eac354e5260324bcf
        OPTIONS "FERN_BUILD_ALGORITHM ON" "FERN_BUILD_TEST ${PCRASTER_BUILD_TEST}" "DEVBASE_BUILD_TEST ${PCRASTER_BUILD_TEST}" "CMAKE_SKIP_INSTALL_RULES ON"
    )
    # Just recreate an empty file to install nothing from Fern when installing PCRaster
    file(TOUCH ${CMAKE_CURRENT_BINARY_DIR}/_deps/fern-build/cmake_install.cmake)
endif()

CPMAddPackage(
  NAME xsd
  GIT_REPOSITORY https://git.codesynthesis.com/xsd/xsd.git
  GIT_TAG 538fb327e13c3c9d3e7ae4a7dd06098d12667f2a
  DOWNLOAD_ONLY YES
)


list(APPEND PCR_QT_COMPONENTS Core Sql Xml)

if(PCRASTER_BUILD_AGUILA)
    list(APPEND PCR_QT_COMPONENTS Gui Widgets Charts)
endif()

if(PCRASTER_WITH_OPENGL)
    find_package(OpenGL REQUIRED)
    list(APPEND PCR_QT_COMPONENTS OpenGL)
endif()

if(PCRASTER_WITH_QT6)
    find_package(Qt6 COMPONENTS ${PCR_QT_COMPONENTS})
endif()

if(NOT Qt6_FOUND)
    find_package(Qt5 5.15 REQUIRED COMPONENTS ${PCR_QT_COMPONENTS})
endif()

message(STATUS "Found Qt: ")
message(STATUS "  version:   " ${Qt5_VERSION}${Qt6_VERSION})
add_compile_definitions(QT_DISABLE_DEPRECATED_BEFORE=0x050F00)


find_package(XercesC REQUIRED)


find_package(GDAL 3.0 REQUIRED)
message(STATUS "Found GDAL: ")
message(STATUS "  version:   " ${GDAL_VERSION})
message(STATUS "  libraries: " ${GDAL_LIBRARIES})
message(STATUS "  includes:  " ${GDAL_INCLUDE_DIRS})
find_program(GDAL_TRANSLATE gdal_translate
    HINTS ${GDAL_INCLUDE_DIRS}/../bin
)
if(EXISTS "${GDAL_INCLUDE_DIRS}/../../share/gdal")
    set(GDAL_DATA "${GDAL_INCLUDE_DIRS}/../../share/gdal")
elseif(EXISTS "${GDAL_INCLUDE_DIRS}/../share/gdal")
    set(GDAL_DATA "${GDAL_INCLUDE_DIRS}/../share/gdal")
else()
    message(FATAL_ERROR "GDAL data dir not found")
endif()

if(PCRASTER_BUILD_TEST)
    if(NOT GDAL_TRANSLATE)
        message(FATAL_ERROR "gdal_translate executable not found")
    endif()
endif()


find_package(Python3 COMPONENTS Interpreter Development NumPy)
message(STATUS "Found Python3:")
message(STATUS "  Interpreter ID: " ${Python3_INTERPRETER_ID})
message(STATUS "    version:      " ${Python3_VERSION})
message(STATUS "    executable:   " ${Python3_EXECUTABLE})
message(STATUS "    site-lib:     " ${Python3_SITELIB})
message(STATUS "    includes:     " ${Python3_INCLUDE_DIRS})
message(STATUS "  NumPy:" )
message(STATUS "    version:      " ${Python3_NumPy_VERSION})
message(STATUS "    includes:     " ${Python3_NumPy_INCLUDE_DIRS})

if(UNIX)
    set(CURSES_NEED_NCURSES TRUE)
    set(CURSES_NEED_WIDE TRUE)
    find_package(Curses REQUIRED)
    message(STATUS "Found ncurses: ")
    message(STATUS "  libraries: " ${CURSES_LIBRARIES})
    message(STATUS "  includes : " ${CURSES_INCLUDE_DIRS})
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
    find_package(Doxygen REQUIRED)
    include(SphinxDoc)
    if(NOT SPHINX_BUILD_EXECUTABLE OR NOT SPHINX_APIDOC_EXECUTABLE)
        message(FATAL_ERROR "sphinx-build not found")
    endif()

    set(SPHINX_HTML_THEME "pyramid")
endif()


# Python.h needs to be known to pass the test
set (CMAKE_REQUIRED_INCLUDES "${Python3_INCLUDE_DIRS};${CMAKE_REQUIRED_INCLUDES}")

check_include_file_cxx("pybind11/pybind11.h" HAVE_PYBIND11)

if(NOT HAVE_PYBIND11)
    FetchContent_Declare(
        pybind11
        GIT_REPOSITORY https://github.com/pybind/pybind11
        GIT_TAG        v2.10.0
    )

    FetchContent_GetProperties(pybind11)
    if(NOT pybind11_POPULATED)
        FetchContent_Populate(pybind11)
        add_subdirectory(${pybind11_SOURCE_DIR} ${pybind11_BINARY_DIR})
    endif()
else()
    find_package(pybind11 REQUIRED)
endif()


# >>> The ones below are required in case a developer needs to
#     regenerate one of the pcraster_model_engine's XML files
# set(DEVBASE_LIB_XSLT_XSLTPROC_REQUIRED TRUE)
# set(DEVBASE_LIB_XML2_REQUIRED TRUE)
# set(DEVBASE_LIB_XSLT_REQUIRED TRUE)
#
# ... or XSD
# set(DEVBASE_XSD_REQUIRED TRUE)
# <<<
