add_library(_pcraster_mldd_python SHARED
    Extension.cc
    Mldd.cc
)

add_library(pcraster::python_mldd ALIAS _pcraster_mldd_python)

fix_boost_python_visibility(_pcraster_mldd_python)

target_include_directories(_pcraster_mldd_python
    PRIVATE
        ${PYTHON_INCLUDE_DIR}
)

target_link_libraries(_pcraster_mldd_python
    PRIVATE
        pcraster::mldd
        pcraster::model_engine
        ${PCR_BOOST_PYTHON}
        ${PYTHON_LIBRARIES}
)

configure_python_extension(_pcraster_mldd_python
    "_pcraster_mldd"
)

set(_TARGET_BIN ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})

set_target_properties(_pcraster_mldd_python
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            "${_TARGET_BIN}/pcraster/mldd"
        LIBRARY_OUTPUT_DIRECTORY
            "${_TARGET_BIN}/pcraster/mldd"
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
#        COMMAND ${CMAKE_COMMAND} -E copy
#            $<TARGET_FILE:_pcraster_mldd_python>
#            ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/mldd
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
