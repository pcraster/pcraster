add_library(_pcraster_moc_python SHARED
    Extension
    Moc
)

add_library(pcraster::moc_python ALIAS _pcraster_moc_python)

fix_boost_python_visibility(_pcraster_moc_python)

target_include_directories(_pcraster_moc_python
    PRIVATE
        ${PYTHON_INCLUDE_DIR}
)

target_link_libraries(_pcraster_moc_python
    PUBLIC
        pcraster::moc
        pcraster::model_engine
        ${PCR_BOOST_PYTHON}
        ${PYTHON_LIBRARIES}
)

configure_python_extension(_pcraster_moc_python
    "_pcraster_moc"
)

set(_TARGET_BIN ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})

set_target_properties(_pcraster_moc_python
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            "${_TARGET_BIN}/pcraster/moc"
        LIBRARY_OUTPUT_DIRECTORY
            "${_TARGET_BIN}/pcraster/moc"
)

add_dependencies(_pcraster_moc_python
    _pcraster
)

add_custom_command(
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
