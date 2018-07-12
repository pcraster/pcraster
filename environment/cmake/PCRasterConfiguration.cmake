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
    FALSE)
option(
    PCRASTER_BUILD_TEST
    "Build tests"
    FALSE)
option(
    PCRASTER_BUILD_BLOCKPYTHON
    "Build blockpython module"
    FALSE)
option(
    PCRASTER_WITH_PYTHON_MULTICORE
    "Build Python multicore module"
    TRUE)
option(
    PCRASTER_WITH_MODFLOW
    "Build Modflow module"
    TRUE)
option(
    PCRASTER_WITH_AGUILA
    "Build Aguila"
    TRUE)

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
    date_time filesystem math_c99 program_options)

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
else()
    # 'Recent' boost
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
endif()

set(DEVBASE_PYTHON_LIBS_REQUIRED TRUE)
set(DEVBASE_NUMPY_REQUIRED TRUE)
find_python_module(numpy REQUIRED)
find_python_module(docopt REQUIRED)

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
