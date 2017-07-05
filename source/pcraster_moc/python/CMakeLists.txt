include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
    ${CMAKE_CURRENT_SOURCE_DIR}/..
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_model_engine
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_dal
)

SET(SOURCES
    Extension.cc
    Moc.cc
)
ADD_LIBRARY(_pcraster_moc_python
    SHARED
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(_pcraster_moc_python
     pcraster_moc
     pcraster_model_engine
     # pcrcom
     # pcrgeo
#    ${Boost_PYTHON_LIBRARY}
    ${PYTHON_LIBRARIES}
    Boost::python
)
CONFIGURE_PYTHON_EXTENSION(_pcraster_moc_python
    "_pcraster_moc"
)

set(_TARGET_BIN ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})

set_target_properties(_pcraster_moc_python
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY "${_TARGET_BIN}/pcraster/moc"
        LIBRARY_OUTPUT_DIRECTORY "${_TARGET_BIN}/pcraster/moc"
)

ADD_DEPENDENCIES(_pcraster_moc_python _pcraster)
ADD_CUSTOM_COMMAND(
    TARGET _pcraster_moc_python
    POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory
            ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/moc
        COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/__init__.py
            ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/moc
#        COMMAND ${CMAKE_COMMAND} -E copy
#            $<TARGET_FILE:_pcraster_moc_python>
#            ${PROJECT_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/pcraster/moc
)
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
