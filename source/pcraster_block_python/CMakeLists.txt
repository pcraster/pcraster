# NO_EXTRAS required for success on macOS conda build
pybind11_add_module(_pcraster_block_python MODULE NO_EXTRAS THIN_LTO
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
)

set_target_properties(_pcraster_block_python
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster
        LIBRARY_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster
        OUTPUT_NAME
            "_pcraster_block_python"
        INSTALL_RPATH
            "$ORIGIN/../../lib"
        DEBUG_POSTFIX
            ""
        PREFIX
            "${PYTHON_MODULE_PREFIX}"
        SUFFIX
            "${PYTHON_MODULE_EXTENSION}"
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

    # Dependencies for the unit tests...
    add_test(
        NAME
            setup_block_python_dependencies
        COMMAND
            "${CMAKE_COMMAND}"
            --build "${CMAKE_BINARY_DIR}"
            --config "$<CONFIG>"
            --target _pcraster _pcraster_block_python
    )

    set_tests_properties(setup_block_python_dependencies
        PROPERTIES
            FIXTURES_SETUP
                FixtureBlockPythonDependencies
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
                "FixtureBlockPythonDependencies;FixtureBlockPython"
    )

    # Environment variables to execute this unit test via ctest properly
    set(test_env
        PATH=path_list_prepend:$<$<PLATFORM_ID:Windows>:$<TARGET_FILE_DIR:pcrcalc>>
        PYTHONPATH=path_list_prepend:${CMAKE_CURRENT_BINARY_DIR}
        PYTHONPATH=path_list_prepend:$<TARGET_FILE_DIR:pcrcalc>
    )

    set_tests_properties(blockpy_blockpy_pythonunittest
        PROPERTIES
            ENVIRONMENT_MODIFICATION "${test_env}"
    )



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
