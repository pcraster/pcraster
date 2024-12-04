add_library(_pcraster_moc_python MODULE
    Extension.cc
    Moc.cc
)

add_library(pcraster::moc_python ALIAS _pcraster_moc_python)

target_link_libraries(_pcraster_moc_python
    PUBLIC
        pcraster::moc
        pcraster::model_engine
        $<IF:$<CXX_COMPILER_ID:GNU>,Python::Python,Python::Module>
)

set_target_properties(_pcraster_moc_python
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/moc
        LIBRARY_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/moc
        OUTPUT_NAME
            "_pcraster_moc"
        INSTALL_RPATH
            "$ORIGIN/../../../lib"
        DEBUG_POSTFIX ""
)

set_target_properties(_pcraster_moc_python
    PROPERTIES
        PREFIX "${PYTHON_MODULE_PREFIX}"
        SUFFIX "${PYTHON_MODULE_EXTENSION}"
        DEBUG_POSTFIX ""
)

add_dependencies(_pcraster_moc_python
    _pcraster
)

add_custom_command(
    TARGET _pcraster_moc_python
    POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/moc
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/__init__.py
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/moc
        # Testdata
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/../../mldd/demo/DemIn
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/test.py
            ${CMAKE_CURRENT_BINARY_DIR}
)


install(
   TARGETS _pcraster_moc_python
   LIBRARY DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/moc
   RUNTIME DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/moc
)

install(
   FILES
       __init__.py
   DESTINATION
      ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/moc
)

if(PCRASTER_BUILD_TEST)

    add_test(
        NAME
            pcraster_moc_python_test
        COMMAND
            ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
    )

    # Environment variables to execute this unit test via ctest properly
    set(test_env
        PATH=path_list_prepend:$<$<PLATFORM_ID:Windows>:$<TARGET_FILE_DIR:pcrcalc>>
        PYTHONPATH=path_list_prepend:${CMAKE_CURRENT_BINARY_DIR}
        PYTHONPATH=path_list_prepend:$<TARGET_FILE_DIR:pcrcalc>
    )

    set_tests_properties(pcraster_moc_python_test
        PROPERTIES
            ENVIRONMENT_MODIFICATION
                "${test_env}"
    )

endif()
