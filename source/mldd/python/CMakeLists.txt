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
        $<IF:$<CXX_COMPILER_ID:GNU>,Python::Python,Python::Module>
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
            "\\\$ORIGIN/../../../lib"
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

set_target_properties(_pcraster_mldd_python
    PROPERTIES
        PREFIX "${PYTHON_MODULE_PREFIX}"
        SUFFIX "${PYTHON_MODULE_EXTENSION}"
)

### SET_TARGET_PROPERTIES(_pcraster_mldd_python
###     PROPERTIES
###         ### SKIP_BUILD_RPATH FALSE
###         ### BUILD_WITH_INSTALL_RPATH TRUE
###         ### INSTALL_RPATH "$ORIGIN/../../../lib"
###         ### INSTALL_RPATH_USE_LINK_PATH FALSE
###
###         # Write the extension to the pcraster sub directory.
###         LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/mldd
###         # Might be needed on Windows:
###         # RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin/pcraster
### )

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

    if(WIN32)
        set(ENV_PATH $ENV{PATH})
        string(REPLACE ";" "\\;" ENV_PATH "${ENV_PATH}")

        set_tests_properties(mldd_python_unittests
            PROPERTIES
                ENVIRONMENT
                    "PATH=${ENV_PATH}\\;$<TARGET_FILE_DIR:pcrcalc>;PYTHONPATH=$<TARGET_FILE_DIR:pcrcalc>"
        )
    else()
        set_tests_properties(mldd_python_unittests
            PROPERTIES
                ENVIRONMENT
                    "PYTHONPATH=$<TARGET_FILE_DIR:pcrcalc>"
        )
    endif()
endif()
