# Options for selecting the modules to build.
# PCRASTER_BUILD_<module>

# Options for selecting features.
# PCRASTER_WITH_<feature>

# option(PCRASTER_BUILD_ALL
#     "Build everything, except for documentation and tests" FALSE)
# option(PCRASTER_WITH_ALL "Support all features" FALSE)

option(
    PCRASTER_BUILD_DOCUMENTATION
    "Build documentation"
    OFF)
option(
    PCRASTER_BUILD_TEST
    "Build tests"
    OFF)
option(
    PCRASTER_BUILD_BLOCKPYTHON
    "Build blockpython module"
    OFF)
option(
    PCRASTER_WITH_PYTHON_MULTICORE
    "Build Python multicore module"
    ON)
option(
    PCRASTER_WITH_MODFLOW
    "Build Modflow module"
    ON)
option(
    PCRASTER_WITH_AGUILA
    "Build Aguila"
    ON)

set(DEVBASE_BOOST_REQUIRED TRUE)


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

set(DEVBASE_OPENGL_REQUIRED TRUE)
set(DEVBASE_QT_REQUIRED TRUE)
set(DEVBASE_REQUIRED_QT_VERSION 5)
set(DEVBASE_REQUIRED_QT_COMPONENTS
    Core Gui OpenGL Sql Widgets Xml)
if(PCRASTER_WITH_AGUILA)
    set(DEVBASE_QWT_REQUIRED TRUE)
endif()

set(DEVBASE_XERCES_REQUIRED TRUE)

set(DEVBASE_GDAL_REQUIRED TRUE)  # Version >= 2.0.0.


find_package(PythonInterp)
find_package(Boost)
if(${Boost_VERSION} LESS 106500)
    # 'Old' boost
    if(${PYTHON_VERSION_STRING} VERSION_LESS 3.0)
        # Python 2
        list(APPEND DEVBASE_REQUIRED_BOOST_COMPONENTS
            python)
        set(PCR_BOOST_PYTHON Boost::python)
        set(PCR_BOOST_PYTHON_NUMPY Boost::python)
    else()
        # Python 3
        list(APPEND DEVBASE_REQUIRED_BOOST_COMPONENTS
            python3)
        set(PCR_BOOST_PYTHON Boost::python3)
        set(PCR_BOOST_PYTHON_NUMPY Boost::python3)
    endif()
elseif( (${Boost_VERSION} GREATER 106400) AND (${Boost_VERSION} LESS 106700))
    if(${PYTHON_VERSION_STRING} VERSION_LESS 3.0)
        # Python 2
        list(APPEND DEVBASE_REQUIRED_BOOST_COMPONENTS
            python numpy)
        set(PCR_BOOST_PYTHON Boost::python)
        set(PCR_BOOST_PYTHON_NUMPY Boost::python Boost::numpy)
    else()
        # Python 3
        list(APPEND DEVBASE_REQUIRED_BOOST_COMPONENTS
            python3 numpy3)
        set(PCR_BOOST_PYTHON Boost::python3)
        set(PCR_BOOST_PYTHON_NUMPY Boost::python3 Boost::numpy3)
    endif()
else()
    list(APPEND DEVBASE_REQUIRED_BOOST_COMPONENTS
            python${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR}
            numpy${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR})
    set(PCR_BOOST_PYTHON Boost::python${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR})
    set(PCR_BOOST_PYTHON_NUMPY Boost::python${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR} Boost::numpy${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR})
endif()

set(DEVBASE_PYTHON_LIBS_REQUIRED TRUE)
set(DEVBASE_NUMPY_REQUIRED TRUE)

find_python_module(numpy REQUIRED)
if(PCRASTER_BUILD_TEST)
    find_python_module(psutil REQUIRED)
endif()

if(WIN32)
    find_package(PDCurses REQUIRED)
    set(CURSES_INCLUDE_DIRS ${PDCURSES_INCLUDE_DIR})
    set(CURSES_LIBRARIES ${PDCURSES_LIBRARIES})
else()
    set(DEVBASE_CURSES_REQUIRED TRUE)
    set(DEVBASE_CURSES_WIDE_CHARACTER_SUPPORT_REQUIRED FALSE)
endif()


if(PCRASTER_BUILD_TEST)
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
