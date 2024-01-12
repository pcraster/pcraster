add_library(_pcraster_moc_python MODULE
    Extension.cc
    Moc.cc
)

add_library(pcraster::moc_python ALIAS _pcraster_moc_python)

target_link_libraries(_pcraster_moc_python
    PUBLIC
        pcraster::moc
        pcraster::model_engine
        pybind11::module
)

# TODO why is Python::module not working on Linux?
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_link_libraries(_pcraster_moc_python
        PRIVATE
            Python::Python
    )
else()
    target_link_libraries(_pcraster_moc_python
        PRIVATE
            Python::Module
    )
endif()

configure_python_extension(_pcraster_moc_python
    "_pcraster_moc"
)

set_target_properties(_pcraster_moc_python
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/moc
        LIBRARY_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/moc
        INSTALL_RPATH
            "\\\$ORIGIN/../../../lib"
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


#install(
#    TARGETS _pcraster_moc_python
#    LIBRARY DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/moc
#    RUNTIME DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/moc
#)

#install(
#    FILES
#        __init__.py
#    DESTINATION
#       ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/moc
#)

#if(PCRASTER_BUILD_TEST)
#
#    add_test(
#        NAME
#            pcraster_moc_python_test
#        COMMAND
#            ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
#    )
#
#    # add path of project's unit test related files to PYTHONPATH
#    # required to execute this unit test via ctest properly
#    if(WIN32)
#        set(ENV_PATH $ENV{PATH})
#        string(REPLACE ";" "\\;" ENV_PATH "${ENV_PATH}")
#        # argument is a 'cmake' ; separated list of values, therefore escape the environment path separator
#        set_tests_properties(pcraster_moc_python_test PROPERTIES
#          ENVIRONMENT "PATH=${ENV_PATH}\\;$<TARGET_FILE_DIR:pcrcalc>;PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;$<TARGET_FILE_DIR:pcrcalc>"
#        )
#    else()
#        set_tests_properties(pcraster_moc_python_test PROPERTIES
#          ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:$<TARGET_FILE_DIR:pcrcalc>"
#        )
#    endif()
#
#endif()


# TODO
# SET_TARGET_PROPERTIES(_pcraster_moc_python
#     PROPERTIES
#         ### SKIP_BUILD_RPATH FALSE
#         ### BUILD_WITH_INSTALL_RPATH TRUE
#         ### INSTALL_RPATH "$ORIGIN/../../../lib"
#         ### INSTALL_RPATH_USE_LINK_PATH FALSE
#
#         # Write the extension to the pcraster sub directory.
#         LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/moc
#         # Might be needed on Windows:
#         RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/moc
# )
