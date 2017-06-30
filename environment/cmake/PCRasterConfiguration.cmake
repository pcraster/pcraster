# Options for selecting the modules to build.
# PCRASTER_BUILD_<module>

# Options for selecting features.
# PCRASTER_WITH_<feature>

# option(PCRASTER_BUILD_ALL
#     "Build everything, except for documentation and tests" FALSE)
# option(PCRASTER_WITH_ALL "Support all features" FALSE)
option(PCRASTER_BUILD_DOCUMENTATION "Build documentation" FALSE)
option(PCRASTER_BUILD_TEST "Build tests" FALSE)
#option(PCRASTER_BUILD_EXPERIMENTAL "Build experimental features" FALSE)
option(PCRASTER_BUILD_BLOCKPYTHON "Build blockpython module" FALSE)
option(PCRASTER_WITH_PYTHON_MULTICORE "Build Python multicore module" FALSE)


# if(PCRASTER_BUILD_ALL)
    set(DEVBASE_BOOST_REQUIRED TRUE)
    list(APPEND DEVBASE_REQUIRED_BOOST_COMPONENTS
        date_time filesystem math_c99 program_options python regex system)
# endif()

set(DEVBASE_OPENGL_REQUIRED TRUE)
set(DEVBASE_QT_REQUIRED TRUE)
set(DEVBASE_REQUIRED_QT_VERSION "5")
set(DEVBASE_REQUIRED_QT_COMPONENTS Core Gui OpenGL Sql Widgets Xml)

set(DEVBASE_QWT_REQUIRED TRUE)

set(DEVBASE_GDAL_REQUIRED TRUE)  # Version >= 2.0.0.

set(DEVBASE_SQLITE_REQUIRED TRUE)

set(DEVBASE_PYTHON_INTERP_REQUIRED TRUE)
# if(PCRASTER_USE_PYTHON_VERSION)
#     # 3.5 2.7
#     set(DEVBASE_REQUIRED_PYTHON_VERSION ${PCRASTER_USE_PYTHON_VERSION})
# endif()
set(DEVBASE_PYTHON_LIBS_REQUIRED TRUE)
set(DEVBASE_NUMPY_REQUIRED TRUE)

set(DEVBASE_XERCES_REQUIRED TRUE)

set(DEVBASE_XSD_REQUIRED TRUE)

if(WIN32)
  find_package(PDCurses REQUIRED)
else()
  set(DEVBASE_CURSES_REQUIRED TRUE)
  set(DEVBASE_CURSES_WIDE_CHARACTER_SUPPORT_REQUIRED FALSE)
endif()

if(PCRASTER_WITH_PYTHON_MULTICORE)
  set(DEVBASE_FERN_REQUIRED TRUE)
endif()


if(PCRASTER_BUILD_TEST)
    set(PCRASTER_TEST_REQUIRED TRUE)
    set(DEVBASE_BOOST_REQUIRED TRUE)
    list(APPEND DEVBASE_REQUIRED_BOOST_COMPONENTS
        unit_test_framework)
    # The ones below are required in case a developer needs to
    # regenerate one of the pcraster_model_engine's XML files
    # set(DEVBASE_LIB_XSLT_XSLTPROC_REQUIRED TRUE)
    # set(DEVBASE_LIB_XML2_REQUIRED TRUE)
    # set(DEVBASE_LIB_XSLT_REQUIRED TRUE)
endif()



if(PCRASTER_TEST_REQUIRED)
    # Used by dal's testrun.prolog.
    set(DEVBASE_SQLITE_EXECUTABLE_REQUIRED TRUE)
endif()


if(PCRASTER_BUILD_DOCUMENTATION)
    set(DEVBASE_DOXYGEN_REQUIRED TRUE)
    set(DEVBASE_SPHINX_REQUIRED TRUE)
    set(PCRASTER_TEST_REQUIRED TRUE)
    set(SPHINX_HTML_THEME "classic")
endif()
