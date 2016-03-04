if(PCRASTER_BUILD_TEST)
  add_subdirectory(cplusplus)
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


# SET(PCRASTER_PACKAGE_BUILD_DIRECTORY
#     ${PCRTREE2_OBJECTS}/bin/${CMAKE_CFG_INTDIR}/pcraster
# )
# SET(COLLECTION_BUILD_DIRECTORY
#     ${PCRASTER_PACKAGE_BUILD_DIRECTORY}/collection
# )
#
# # This enables us to test and use the code as if it where installed already.
# # Dummy target that is always built, but does nothing. This enables us to
# # attach post-build commands.
# ADD_CUSTOM_TARGET(arrayed_variables ALL)
# ADD_CUSTOM_COMMAND(
#     TARGET arrayed_variables
#     POST_BUILD
#         COMMAND cmake -E remove_directory ${COLLECTION_BUILD_DIRECTORY}
#         COMMAND cmake -E make_directory ${COLLECTION_BUILD_DIRECTORY}
#         COMMAND cp ${SOURCES} ${COLLECTION_BUILD_DIRECTORY}
#         WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
# )
