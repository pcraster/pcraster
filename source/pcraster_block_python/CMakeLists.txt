include_directories(
    # ${CMAKE_CURRENT_SOURCE_DIR}/../include
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
ADD_LIBRARY(_pcraster_block_python
    SHARED
    ${SOURCES}
)
if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    # otherwise compilation with hidden visibility flags will fail
    if(${Boost_MINOR_VERSION} EQUAL 65)
        target_compile_definitions(
            _pcraster_block_python
        PUBLIC
            -DBOOST_PYTHON_USE_GCC_SYMBOL_VISIBILITY)
    endif()
endif()
TARGET_LINK_LIBRARIES(_pcraster_block_python
    pcrdiscr
    pcrblock
    pcrraster
    pcrvoxelstack
    pcraster_model_engine
#    ${Boost_PYTHON_LIBRARY}
#    Boost::python
    ${PCR_BOOST_PYTHON}
    ${PYTHON_LIBRARIES}
)
CONFIGURE_PYTHON_EXTENSION(_pcraster_block_python
    "_pcraster_block_python"
)


set(_TARGET_BIN ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})

set_target_properties(_pcraster_block_python
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY "${_TARGET_BIN}/pcraster"
        LIBRARY_OUTPUT_DIRECTORY "${_TARGET_BIN}/pcraster"
)

install(
  FILES
    PCRasterBlock.py
  DESTINATION
    "python/pcraster"
)


if(PCRASTER_BUILD_TEST)

    configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/test_prolog.py.in
        ${CMAKE_CURRENT_BINARY_DIR}/test_prolog.py
        @ONLY
    )

    add_test(
        NAME setup_block_python
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_block_python
        PROPERTIES FIXTURES_SETUP FixtureBlockPython
    )

  add_test(
    NAME
         blockpy_blockpy_pythonunittest
    COMMAND
        ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
  )

    set_tests_properties(blockpy_blockpy_pythonunittest PROPERTIES
        FIXTURES_REQUIRED
          FixtureBlockPython
    )




#   set_tests_properties(blockpy_blockpy_pythonunittest PROPERTIES
#     DEPENDS
#       _pcraster_block_python
#       python_modelling_framework_dummy
#   )


#   set(TEST_NAMES
#     blockpy_pythonunittest
#   )
#
#   add_unit_tests(
#     SCOPE blockpy
#     NAMES ${TEST_NAMES}
#     LINK_LIBRARIES
#       ${Boost_PYTHON_LIBRARY}
#       ${PYTHON_LIBRARIES}
#   )

  # add path of project's unit test related files to PYTHONPATH
  # required to execute this unit test via ctest properly
  if(WIN32)
    set(ENV_PATH $ENV{PATH})
    string(REPLACE ";" "\\;" ENV_PATH "${ENV_PATH}")
    # argument is a 'cmake' ; separated list of values, therefore escape the environment path separator
    set_tests_properties(blockpy_blockpy_pythonunittest PROPERTIES
      ENVIRONMENT "PATH=${ENV_PATH}\\;${EXECUTABLE_OUTPUT_PATH};PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;${EXECUTABLE_OUTPUT_PATH}"
    )
  else()
    set_tests_properties(blockpy_blockpy_pythonunittest PROPERTIES
      ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:${EXECUTABLE_OUTPUT_PATH}"
    )
  endif()




# OLS: clean up the comments when refacoring for the new windows build
# I leave them here just as info in case of troubles...

#
#     ADD_LIBRARY(pcraster_block_python_static
#         STATIC
#         ${SOURCES}
#     )
#     SET(TEST_SOURCES
#         blockpy_pythonunittest.cc
#         blockpy_test.cc
#     )
#     ADD_EXECUTABLE(test_pcraster_block_python
#         ${TEST_SOURCES}
#     )
#
#     TARGET_LINK_LIBRARIES(test_pcraster_block_python
#         pcraster_block_python_static
#         # ${_LIBRARIES_TO_LINK}
#     )
#
#     ADD_CUSTOM_TARGET(run_test_pcraster_block_python
#         COMMAND bash ./testrun.prolog
#         COMMAND test_pcraster_block_python --detect_memory_leaks=0
#         DEPENDS test_pcraster_block_python
#         WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
#     )
#     ADD_DEPENDENCIES(tests run_test_pcraster_block_python)
endif()


#----------------------------------------------------------
#INSTALL(
#  TARGETS pcraster_block_python
#  LIBRARY DESTINATION PCRasterPython
#  RUNTIME DESTINATION PCRasterPython
#)

