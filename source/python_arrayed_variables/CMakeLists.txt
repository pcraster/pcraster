if(PCRASTER_BUILD_TEST)
    add_subdirectory(UnitTests)
endif()

set(SOURCES
    __init__.py
    Index.py
    VariableCollection.py
)

install(
    FILES ${SOURCES}
    DESTINATION python/pcraster/collection
)

set(COLLECTION_BUILD_DIRECTORY
    ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}/pcraster/collection
)

# This enables us to test and use the code as if it where installed already.
# Dummy target that is always built, but does nothing. This enables us to
# attach post-build commands.
add_custom_target(python_arrayed_variables_dummy)

add_dependencies(python_arrayed_variables_dummy
    _pcraster_modflow_python
)

add_custom_command(
    TARGET python_arrayed_variables_dummy
    POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E remove_directory ${COLLECTION_BUILD_DIRECTORY}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${COLLECTION_BUILD_DIRECTORY}
        COMMAND ${CMAKE_COMMAND} -E copy ${SOURCES} ${COLLECTION_BUILD_DIRECTORY}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS ${SOURCES}
)
