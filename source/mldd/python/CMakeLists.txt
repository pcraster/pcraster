include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
    ${CMAKE_CURRENT_SOURCE_DIR}/..
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_model_engine
)

ADD_LIBRARY(_pcraster_mldd_python
    SHARED
    Extension.cc
    Mldd.cc
)
TARGET_LINK_LIBRARIES(_pcraster_mldd_python
    mldd # _static
    pcraster_model_engine
    # pcrcom
    # pcrgeo
    # misc
    # ${PCRASTER_DAL_LIBRARY}
    ${Boost_PYTHON_LIBRARY}
    # ${BOOST_FILESYSTEM_LIBRARY}
    ${PYTHON_LIBRARIES}
)
CONFIGURE_PYTHON_EXTENSION(_pcraster_mldd_python
    "_pcraster_mldd"
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
ADD_DEPENDENCIES(_pcraster_mldd_python _pcraster)
ADD_CUSTOM_COMMAND(
    TARGET _pcraster_mldd_python
    POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory
            ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/mldd
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/__init__.py
            ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/mldd
        COMMAND ${CMAKE_COMMAND} -E copy
            $<TARGET_FILE:_pcraster_mldd_python>
            ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/mldd
)
