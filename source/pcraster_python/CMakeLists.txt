add_subdirectory(utils)

configure_file(
    operations.inc.in
    operations.inc
    @ONLY
    NEWLINE_STYLE UNIX
)

configure_file(
    operations.py.in
    operations.py
    @ONLY
    NEWLINE_STYLE UNIX
)

# operations.inc and operations.py were previously generated each time at a build.
# Due to removal of xsltproc dependency this file needs to be
# regenerated when one of the source files change
# Hash the copied file in build directory rather than the source due to different UNIX/WIN line endings

# check operationExpanded.xml as it's originating from operations.xml
file(SHA1 ${CMAKE_CURRENT_BINARY_DIR}/../pcraster_model_engine/operationExpanded.xml op_expanded)
# and these related to the Operation*xsl ...
file(SHA1 ${CMAKE_CURRENT_BINARY_DIR}/operations.inc operations_inc_checksum)
file(SHA1 ${CMAKE_CURRENT_BINARY_DIR}/operations.py operations_py_checksum)

if( ( NOT ${op_expanded} STREQUAL "c1d7687a22a7e88a3637515554dd54e4f3779c2d" ) OR
    ( NOT ${operations_inc_checksum} STREQUAL "a7f4dc5eabd251bc6b12c1489f49dc68d7be9aa4" ) OR
    ( NOT ${operations_py_checksum} STREQUAL "6827dcdd46a9848aa4b80d3f0cfac1d2335dce1f" )
  )
    message(FATAL_ERROR "The files operations.inc or operations.py are older than one of their dependencies. Update operations.inc.in or operations.py.in first; or contact the PCRaster Research and Development team in case you don't know how to proceed.")
    # This is the previously executed command, results in something
    # similar to this:
    # xsltproc --path ../xslt OperationsToWrappers.xsl ../pcraster_model_engine/operation.xml
    #
    # ADD_CUSTOM_COMMAND(
    #     OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/operations.inc
    #     DEPENDS
    #         ${CMAKE_CURRENT_SOURCE_DIR}/OperationsToWrappers.xsl
    #         ${CMAKE_CURRENT_SOURCE_DIR}/OperationToWrapper.xsl
    #         ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_model_engine/operation.xml
    #         WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    #         # why does ${CMAKE_COMMAND} -E copy not work here?
    #         COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/*.xsl ${CMAKE_CURRENT_BINARY_DIR}
    #         COMMAND ${LIBXSLT_XSLTPROC_EXECUTABLE} --path ${CMAKE_CURRENT_SOURCE_DIR}/../xslt ${CMAKE_CURRENT_BINARY_DIR}/OperationsToWrappers.xsl ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_model_engine/operation.xml
    # )
endif()

add_library(_pcraster SHARED
    numpy_conversion
    PCRaster.cc
    value_scale_traits
    pickle
    # operations.inc needs to be manually updated in case the
    # operation.xml, OperationToWrapper.xsl or
    # OperationsToWrappers.xsl change
    operations.inc
)

add_library(pcraster::python ALIAS _pcraster)

fix_boost_python_visibility(_pcraster)

generate_export_header(_pcraster
  BASE_NAME pcraster_python
)

target_include_directories(_pcraster
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)
# workaround to ignore warnings of these headers
target_include_directories(_pcraster SYSTEM
     PRIVATE
        ${NUMPY_INCLUDE_DIRS}
        ${PYTHON_INCLUDE_DIR}
)

target_link_libraries(_pcraster
    PUBLIC
        pcraster::python::utils
        ${PCR_BOOST_PYTHON_NUMPY}
        ${PCR_BOOST_PYTHON}
        ${PYTHON_LIBRARIES}
)

configure_python_extension(_pcraster
    "_pcraster"
)

set(_TARGET_BIN ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})

set_target_properties(_pcraster
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            "${_TARGET_BIN}/pcraster"
        LIBRARY_OUTPUT_DIRECTORY
            "${_TARGET_BIN}/pcraster"
)

### #set_target_properties(_pcraster_modflow_python
### #    PROPERTIES
### #        LIBRARY_OUTPUT_DIRECTORY "${_TARGET_BIN}/pcraster"
### #)


# Determine bitness at compile time...
MATH(EXPR NR_BITS "${CMAKE_SIZEOF_VOID_P} * 8")

# ... and use this for import checks
configure_file(
    __init__.py.in
    ${_TARGET_BIN}/pcraster/__init__.py
)


add_custom_command(
    OUTPUT ${_TARGET_BIN}/pcraster/numpy_operations.py

    COMMAND ${CMAKE_COMMAND} -E copy_directory
        ${CMAKE_CURRENT_SOURCE_DIR}/pcraster ${_TARGET_BIN}/pcraster
    # COMMAND ${CMAKE_COMMAND} -E copy
    #     $<TARGET_FILE:_pcraster> ${_TARGET_BIN}/pcraster
    # COMMAND ${CMAKE_COMMAND} -E copy
    #     $<TARGET_FILE:_pcraster_modflow_python> ${_TARGET_BIN}/pcraster
    # COMMAND ${CMAKE_COMMAND} -E remove $<TARGET_FILE:_pcraster>
    # COMMAND ${CMAKE_COMMAND} -E remove $<TARGET_FILE:_pcraster_modflow_python>

    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_CURRENT_BINARY_DIR}/operations.py ${_TARGET_BIN}/pcraster
    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_CURRENT_SOURCE_DIR}/pcraster/operators.py
        ${_TARGET_BIN}/pcraster

    DEPENDS
        _pcraster
        #_pcraster_modflow_python
)

# TODO This target is used by modflow too. While this CMake file depends on
#      modflow targets. Refactor.
add_custom_target(
    fill_python_directory_structure ALL
    DEPENDS
        ${_TARGET_BIN}/pcraster/numpy_operations.py
)

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    add_test(
        NAME
            python_pcrcalcpy_pythonunittest
        COMMAND
            ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
    )

    add_test(
        NAME setup_pcraster_python
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_pcraster_python
        PROPERTIES
            FIXTURES_SETUP
                FixturePcrasterPython
    )

    set_tests_properties(python_pcrcalcpy_pythonunittest
        PROPERTIES
            FIXTURES_REQUIRED
                FixturePcrasterPython
    )


# OLS: clean up the comments when refacoring for the new windows build
# I leave them here just as info in case of troubles...

#    set(TEST_NAMES
#      pcrcalcpy_pythonunittest
#    )
#
#    add_unit_tests(
#      SCOPE python
#      NAMES ${TEST_NAMES}
#      LINK_LIBRARIES
#        ${Boost_PYTHON_LIBRARY}
#        ${PYTHON_LIBRARIES}
#    )

    # add path of project's unit test related files to PYTHONPATH
    # required to execute this unit test via ctest properly
    if(WIN32)
        set(ENV_PATH $ENV{PATH})
        string(REPLACE ";" "\\;" ENV_PATH "${ENV_PATH}")
        # argument is a 'cmake' ; separated list of values, therefore escape the environment path separator
        set_tests_properties(python_pcrcalcpy_pythonunittest
            PROPERTIES
                ENVIRONMENT
                    "PATH=${ENV_PATH}\\;${EXECUTABLE_OUTPUT_PATH};PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;${EXECUTABLE_OUTPUT_PATH}"
        )
    else()
        set_tests_properties(python_pcrcalcpy_pythonunittest
            PROPERTIES
                ENVIRONMENT
                    "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:${EXECUTABLE_OUTPUT_PATH}"
        )
    endif()


#    add_dependencies(python_pcrcalcpy_pythonunittest
#      examples.sentinel
#      mapattr asc2map col2map
#      _pcraster fill_python_directory_structure
#    )


 # do not run debug tests on Windows
 # or figure out how calling Python_d.exe from here works

    # Unit testing stuff ----------------------------------------------------------
    # Python extension is renamed and linking the unit test executable fails because
    # of this. Dunno how to solve this. For now, just make a static lib to link
    # the unit test against.
#      ADD_LIBRARY(pcraster_python_static STATIC
#          ${PCRASTER_SOURCES}
#          ${CORE_SOURCES}
#      )
#      ADD_DEPENDENCIES(pcraster_python_static
#          _pcraster
#          pcraster_model_engine
#      )


#     SET(TEST_SOURCES
#         pcrcalcpy_test.cc
#         pcrcalcpy_pythonunittest.cc
#     )

 # OLS: interesting, no header files required to compile this?
#  target_include_directories(python_pcrcalcpy_pythonunittest
#    PRIVATE
#     ${CMAKE_CURRENT_SOURCE_DIR}/../include
#     ${CMAKE_CURRENT_SOURCE_DIR}/../app
#     ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
#     ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
#     ${CMAKE_CURRENT_SOURCE_DIR}/../pcrdisc
#     ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
#     ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
#     ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_model_engine
#     ${CMAKE_CURRENT_BINARY_DIR}
    # for major_op.h:
 #   ${CMAKE_CURRENT_BINARY_DIR}/../pcraster_model_engine
#  )

 #  ADD_FILE_DEPENDENCY(pcrcalcpy_pythonunittest.cc ${CMAKE_CURRENT_SOURCE_DIR}/test.py)
  #  ADD_FILE_DEPENDENCY(pcrcalcpy_pythonunittest.cc ${CMAKE_CURRENT_SOURCE_DIR}/testcase.py)
    ### ADD_FILE_DEPENDENCY(pcrcalcpy_pythonunittest.cc ${CMAKE_CURRENT_SOURCE_DIR}/testexamples.py) # generated


    ### SET(_EXAMPLES_DIR ${PROJECT_SOURCE_DIR}/Documentation/PCRasterManual/examples)
    ### ADD_CUSTOM_COMMAND(
    ###   OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/testexamples.py
    ###   DEPENDS
    ###     ${_EXAMPLES_DIR}/ScriptSamples.py
    ###     ${_EXAMPLES_DIR}/PCRasterSample.py
    ###     ${_EXAMPLES_DIR}/Makefile
    ###   COMMAND make -C ${_EXAMPLES_DIR} PythonTests OUTPUT=${CMAKE_CURRENT_SOURCE_DIR}/testexamples.py
    ### )

#     ADD_EXECUTABLE(test_pcraster_python
#         ${TEST_SOURCES}
#     )

#
#     # input files from _EXAMPLES_DIR need these apps:
#      ADD_DEPENDENCIES(python_pcrcalcpy_pythonunittest
#          mapattr asc2map col2map)
#
#      ADD_DEPENDENCIES(python_pcrcalcpy_pythonunittest _pcraster fill_python_directory_structure)

#     IF(NOT (WIN32 AND (CMAKE_BUILD_TYPE STREQUAL Debug)))
#         # This won't work unless a debug numpy is installed.
#         ADD_DEPENDENCIES(tests run_test_pcraster_python)
#     ENDIF()

endif()


#----------------------------------------------------------
#INSTALL(
#  TARGETS _pcraster
#  LIBRARY DESTINATION python/pcraster
#  RUNTIME DESTINATION python/pcraster
#)
#INSTALL(
#  TARGETS _numpy
#  LIBRARY DESTINATION python/pcraster/numpy
#  RUNTIME DESTINATION python/pcraster/numpy
#)
# This should be done at install time, but I can't seem to get
# INSTALL(CODE "...") to call a macro.
### CHECK_RPATH_EQUALS("$ORIGIN" ${PYTHON_LIBRARIES})
### INSTALL_EXTERN_SHARED_LIBRARIES(
###   ${PYTHON_LIBRARIES}
###   )
INSTALL(
    FILES
        pcraster/operators.py
        ${CMAKE_CURRENT_BINARY_DIR}/operations.py
        ${PROJECT_SOURCE_DIR}/environment/script/forkscript.py
        ${PROJECT_SOURCE_DIR}/environment/script/pcr.py
        ${PROJECT_SOURCE_DIR}/environment/script/pcrstat.py
        ${PROJECT_SOURCE_DIR}/environment/script/shellscript.py
        ${PROJECT_SOURCE_DIR}/environment/script/utils.py
    DESTINATION
        python/pcraster
)
### # for backward compatibility
### INSTALL(
###     FILES
###         pcraster.py
###     DESTINATION python
### )
# this directory from Pcrtree2/bin contains the so files
# and the package layout

# CMAKE_CONFIGURATION_TYPES is only defined if the generator supports multiple
# build configurations. In that case the output is stored in a subdirectory
# named after the current build configuration.
IF(CMAKE_CONFIGURATION_TYPES)
    SET(OUTPUT_PATH ${EXECUTABLE_OUTPUT_PATH}/\${CMAKE_INSTALL_CONFIG_NAME})
ELSE()
    SET(OUTPUT_PATH ${EXECUTABLE_OUTPUT_PATH})
ENDIF()

install(
    DIRECTORY
        ${OUTPUT_PATH}/pcraster
        # ${EXECUTABLE_OUTPUT_PATH}/\${CMAKE_INSTALL_CONFIG_NAME}/pcraster
        # ${_TARGET_BIN}/pcraster
        # TODO This doesn't work on Windows.
        # ${_TARGET_BIN}/${CMAKE_CFG_INTDIR}/pcraster
        # ${_TARGET_BIN}/Release/pcraster
    DESTINATION
        python
    PATTERN ".svn" EXCLUDE
    PATTERN "*.pyc" EXCLUDE
)
