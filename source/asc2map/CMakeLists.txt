include_directories(
    # ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
)

SET(SOURCES
    arcgrid.c
    asc2map.c
    main.c
)

ADD_EXECUTABLE(asc2map
    ${SOURCES}
)

# IF(WIN32)
#     TARGET_LINK_LIBRARIES(asc2map
#         app
#         pcrcom
#         mathx
#         misc
#         ${PCRASTER_RASTERFORMAT_LIBRARY}
#         ${BOOST_FILESYSTEM_LIBRARY}
#         ${BOOST_SYSTEM_LIBRARY}
#     )
# ELSE()
TARGET_LINK_LIBRARIES(asc2map
    app
    # pcrcom
    # mathx
    misc
    pcraster_raster_format
)
SET_TARGET_PROPERTIES(asc2map PROPERTIES LINKER_LANGUAGE CXX)
# ENDIF()

INSTALL(
    TARGETS asc2map
    RUNTIME DESTINATION bin COMPONENT Runtime
)
