include_directories(
    # ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
)

SET(SOURCES
    map2col.c
    main.c
)

ADD_EXECUTABLE(map2col
    ${SOURCES}
)

# IF(WIN32)
#     TARGET_LINK_LIBRARIES(map2col
#         app
#         pcrcom
#         mathx
#         misc
#         ${PCRASTER_RASTERFORMAT_LIBRARY}
#         ${BOOST_FILESYSTEM_LIBRARY}
#         ${BOOST_SYSTEM_LIBRARY}
#     )
# ELSE(WIN32)
    TARGET_LINK_LIBRARIES(map2col
        app
        # pcrcom
        # mathx
        misc
        ${PCRASTER_RASTER_FORMAT_LIBRARIES}
    )
    SET_TARGET_PROPERTIES(map2col PROPERTIES LINKER_LANGUAGE CXX)
# ENDIF(WIN32)

INSTALL(
    TARGETS map2col
    RUNTIME DESTINATION bin COMPONENT Runtime
)
