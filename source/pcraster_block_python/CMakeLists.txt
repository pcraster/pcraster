if(CMAKE_COMPILER_IS_GNUCXX)
    SET_SOURCE_FILES_PROPERTIES(blockpy_blockpy.cc
        COMPILE_FLAGS
            "-Wno-strict-aliasing"
    )
endif()

# NO_EXTRAS required for success on macOS conda build
# pybind11_add_module NO_EXTRAS
add_library(_pcraster_block_python MODULE
    blockpy_blockpy.cc
    blockpy_conversions.cc
    blockpy_functions.cc
)

add_library(pcraster::block_python ALIAS _pcraster_block_python)

target_link_libraries(_pcraster_block_python
    PRIVATE
        pcraster::pcrblock
        pcraster::pcrraster
        pcraster::pcrvoxelstack
        pcraster::model_engine
        pybind11::module
)

# TODO why is Python::module not working on Linux?
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_link_libraries(_pcraster_block_python
        PRIVATE
            Python::Python
    )
else()
    target_link_libraries(_pcraster_block_python
        PRIVATE
            Python::Module
    )
endif()

set_target_properties(_pcraster_block_python
    PROPERTIES
        PREFIX "${PYTHON_MODULE_PREFIX}"
        SUFFIX "${PYTHON_MODULE_EXTENSION}"
        DEBUG_POSTFIX ""
)

# configure_python_extension(_pcraster_block_python
#     "_pcraster_block_python"
# )

set(_TARGET_BIN ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})


set_target_properties(_pcraster_block_python
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster
        LIBRARY_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster
        INSTALL_RPATH
            "\\\$ORIGIN/../../lib"
)


install(
    TARGETS _pcraster_block_python
    LIBRARY DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/
    RUNTIME DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/
)

install(
    FILES
        PCRasterBlock.py
    DESTINATION
       ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster
)

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    add_test(
        NAME setup_block_python
        COMMAND ${Python_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_block_python
        PROPERTIES
            FIXTURES_SETUP FixtureBlockPython
    )

    add_test(
        NAME
            blockpy_blockpy_pythonunittest
        COMMAND
            ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
    )

    set_tests_properties(blockpy_blockpy_pythonunittest
        PROPERTIES
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
          ENVIRONMENT "PATH=${ENV_PATH}\\;$<TARGET_FILE_DIR:pcrcalc>;PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;$<TARGET_FILE_DIR:pcrcalc>"
        )
    else()
        set_tests_properties(blockpy_blockpy_pythonunittest PROPERTIES
          ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:$<TARGET_FILE_DIR:pcrcalc>"
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
