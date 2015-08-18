CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrblock
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrdiscr
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrraster
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrvoxelstack
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_model_engine
)

# SET(_LIBRARIES_TO_LINK
#     pcrdiscr
#     pcrblock
#     pcrraster
#     pcrvoxelstack
#     pcraster_model_engine
#     # ${PCRASTER_DAL_LIBRARY}
#     # ${BOOST_PYTHON_LIBRARY}
#     # ${BOOST_SIGNALS_LIBRARY}
#     # ${PYTHON_LIBRARIES}
# )
SET(SOURCES
    blockpy_blockpy.cc
    blockpy_conversions.cc
    blockpy_functions.cc
)
IF(CMAKE_COMPILER_IS_GNUCXX)
    SET_SOURCE_FILES_PROPERTIES(blockpy_blockpy.cc
        COMPILE_FLAGS
            "-Wno-strict-aliasing"
    )
ENDIF()
ADD_LIBRARY(pcraster_block_python
    SHARED
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(pcraster_block_python
    pcrdiscr
    pcrblock
    pcrraster
    pcrvoxelstack
    pcraster_model_engine
    ${Boost_PYTHON_LIBRARY}
    ${PYTHON_LIBRARIES}
)
CONFIGURE_PYTHON_EXTENSION(pcraster_block_python
    "pcraster_block_python"
)


# Unit testing stuff --------------------------------------------------------
ADD_LIBRARY(pcraster_block_python_static
    STATIC
    ${SOURCES}
)
SET(TEST_SOURCES
    blockpy_pythonunittest.cc
    blockpy_test.cc
)
ADD_EXECUTABLE(test_pcraster_block_python
    ${TEST_SOURCES}
)

TARGET_LINK_LIBRARIES(test_pcraster_block_python
    pcraster_block_python_static
    # ${_LIBRARIES_TO_LINK}
)

ADD_CUSTOM_TARGET(run_test_pcraster_block_python
    COMMAND bash ./testrun.prolog
    COMMAND test_pcraster_block_python --detect_memory_leaks=0
    DEPENDS test_pcraster_block_python
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
ADD_DEPENDENCIES(tests run_test_pcraster_block_python)

#----------------------------------------------------------
#INSTALL(
#  TARGETS pcraster_block_python
#  LIBRARY DESTINATION PCRasterPython
#  RUNTIME DESTINATION PCRasterPython
#)
#INSTALL(FILES PCRasterBlock.py
#  DESTINATION PCRasterPython
#)
