# NO_EXTRAS required for success on macOS conda build
pybind11_add_module(_pcraster_mldd_python MODULE NO_EXTRAS THIN_LTO
    Extension.cc
    Mldd.cc
)

add_library(pcraster::python_mldd ALIAS _pcraster_mldd_python)

target_link_libraries(_pcraster_mldd_python
    PRIVATE
        pcraster::mldd
        pcraster::model_engine
)

set_target_properties(_pcraster_mldd_python
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/mldd
        LIBRARY_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/mldd
        OUTPUT_NAME
            "_pcraster_mldd"
        DEBUG_POSTFIX
            ""
        INSTALL_RPATH
            "$ORIGIN/../../../lib"
        PREFIX
            "${PYTHON_MODULE_PREFIX}"
        SUFFIX
            "${PYTHON_MODULE_EXTENSION}"
)

add_dependencies(_pcraster_mldd_python _pcraster)

add_custom_command(
    TARGET _pcraster_mldd_python
    POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory
            ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/mldd
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/__init__.py
            ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/mldd
        # unit test stuff...
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/test.py
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${CMAKE_COMMAND} -E copy_directory
            ${CMAKE_CURRENT_SOURCE_DIR}/../demo
            ${CMAKE_CURRENT_BINARY_DIR}/demo
#        COMMAND ${CMAKE_COMMAND} -E copy
#            $<TARGET_FILE:_pcraster_mldd_python>
#            ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/mldd
)


install(
    TARGETS _pcraster_mldd_python
    LIBRARY DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/mldd
    RUNTIME DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/mldd
)

install(
    FILES
        $<TARGET_FILE_DIR:pcrcalc>/pcraster/mldd/__init__.py
    DESTINATION
        ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/mldd
)

if(PCRASTER_BUILD_TEST)

    add_test(
        NAME
            mldd_python_unittests
        COMMAND
            ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
    )

    # Environment variables to execute this unit test via ctest properly
    set(test_env
        PATH=path_list_prepend:$<$<PLATFORM_ID:Windows>:$<TARGET_FILE_DIR:pcrcalc>>
        PYTHONPATH=path_list_prepend:$<TARGET_FILE_DIR:pcrcalc>
    )

    set_tests_properties(mldd_python_unittests
        PROPERTIES
            ENVIRONMENT_MODIFICATION
                    "${test_env}"
    )

endif()
