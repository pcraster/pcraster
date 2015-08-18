ADD_SUBDIRECTORY(documentation)

CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
    NEWLINE_STYLE UNIX
)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrdisc
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_model_engine
    ${CMAKE_CURRENT_BINARY_DIR}
    # for major_op.h:
    ${CMAKE_CURRENT_BINARY_DIR}/../pcraster_model_engine
)

# SET(_LIBRARIES_TO_LINK
#     pcrcom
#     pcrgeo
#     pcraster_model_engine
#     ${PCRASTER_DAL_LIBRARY}
#     ${BOOST_PYTHON_LIBRARY}
#     ${PYTHON_LIBRARIES}
# )

ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/operations.inc
    DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/OperationsToWrappers.xsl
        ${CMAKE_CURRENT_SOURCE_DIR}/OperationToWrapper.xsl
        ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_model_engine/operation.xml
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        # why does ${CMAKE_COMMAND} -E copy not work here?
        COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/*.xsl ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${LIBXSLT_XSLTPROC_EXECUTABLE} --path ${CMAKE_CURRENT_SOURCE_DIR}/../xslt ${CMAKE_CURRENT_BINARY_DIR}/OperationsToWrappers.xsl ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_model_engine/operation.xml
)

SET(PCRASTER_SOURCES
    Globals.cc
    numpy_conversion
    operations.inc
    PCRaster.cc
    pcrcalcpy_util.cc
    value_scale_traits
    pickle
)
ADD_LIBRARY(_pcraster
    SHARED
    ${PCRASTER_SOURCES}
)
TARGET_LINK_LIBRARIES(_pcraster
    # ${_LIBRARIES_TO_LINK}
    # pcrcom
    # pcrgeo
    pcraster_model_engine
    # pcraster_dal
    # ${PCRASTER_DAL_LIBRARY}
    ${Boost_PYTHON_LIBRARY}
    ${Boost_SYSTEM_LIBRARY}
    ${PYTHON_LIBRARIES}
)
CONFIGURE_PYTHON_EXTENSION(_pcraster
    "_pcraster"
)


SET(_TARGET_BIN ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})

ADD_CUSTOM_COMMAND(
    OUTPUT ${_TARGET_BIN}/pcraster/__init__.py

    COMMAND ${CMAKE_COMMAND} -E copy_directory
        ${CMAKE_CURRENT_SOURCE_DIR}/pcraster ${_TARGET_BIN}/pcraster
    COMMAND ${CMAKE_COMMAND} -E copy
        $<TARGET_FILE:_pcraster> ${_TARGET_BIN}/pcraster
    COMMAND ${CMAKE_COMMAND} -E copy
        $<TARGET_FILE:_pcraster_modflow_python> ${_TARGET_BIN}/pcraster
    COMMAND ${CMAKE_COMMAND} -E remove $<TARGET_FILE:_pcraster>
    COMMAND ${CMAKE_COMMAND} -E remove $<TARGET_FILE:_pcraster_modflow_python>


    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_CURRENT_BINARY_DIR}/operations.py ${_TARGET_BIN}/pcraster
    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_CURRENT_SOURCE_DIR}/pcraster/operators.py
        ${_TARGET_BIN}/pcraster

    DEPENDS
        _pcraster
        _pcraster_modflow_python
)

# TODO This target is used by modflow too. While this CMake file depends on
#      modflow targets. Refactor.
ADD_CUSTOM_TARGET(
    fill_python_directory_structure ALL
    DEPENDS
        ${_TARGET_BIN}/pcraster/__init__.py
)

# Unit testing stuff ----------------------------------------------------------
# Python extension is renamed and linking the unit test executable fails because
# of this. Dunno how to solve this. For now, just make a static lib to link
# the unit test against.
ADD_LIBRARY(pcraster_python_static STATIC
    ${PCRASTER_SOURCES}
    ${CORE_SOURCES}
)
SET(TEST_SOURCES
    pcrcalcpy_test.cc
    pcrcalcpy_pythonunittest.cc
)

ADD_FILE_DEPENDENCY(pcrcalcpy_pythonunittest.cc ${CMAKE_CURRENT_SOURCE_DIR}/test.py)
ADD_FILE_DEPENDENCY(pcrcalcpy_pythonunittest.cc ${CMAKE_CURRENT_SOURCE_DIR}/testcase.py)
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

ADD_EXECUTABLE(test_pcraster_python
    ${TEST_SOURCES}
)

ADD_DEPENDENCIES(test_pcraster_python
    examples.sentinel)

# input files from _EXAMPLES_DIR need these apps:
ADD_DEPENDENCIES(test_pcraster_python mapattr asc2map)

TARGET_LINK_LIBRARIES(test_pcraster_python
    pcraster_python_static
    # ${_LIBRARIES_TO_LINK}
    ${Boost_PYTHON_LIBRARY}
    # ${Boost_SYSTEM_LIBRARY}
    ${PYTHON_LIBRARIES}
)

ADD_CUSTOM_TARGET(run_test_pcraster_python
    COMMAND bash ./testrun.prolog
    COMMAND test_pcraster_python --detect_memory_leaks=0
    DEPENDS test_pcraster_python fill_python_directory_structure
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
IF(NOT (WIN32 AND (CMAKE_BUILD_TYPE STREQUAL Debug)))
    # This won't work unless a debug numpy is installed.
    ADD_DEPENDENCIES(tests run_test_pcraster_python)
ENDIF()

ADD_DEPENDENCIES(pcraster_python_static
    _pcraster
    pcraster_model_engine
)

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
        ${PROJECT_SOURCE_DIR}/environment/script/system.py
        ${PROJECT_SOURCE_DIR}/environment/script/utils.py
    DESTINATION python/pcraster
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

INSTALL(
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
