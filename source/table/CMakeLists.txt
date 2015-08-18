include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../tab
)

SET(SOURCES
    main.c
    move.c
)

ADD_EXECUTABLE(table
    ${SOURCES}
)

# IF(WIN32)
#     TARGET_LINK_LIBRARIES(table
#         api
#         tab
#         app
#         pcrcom
#         mathx
#         misc
#         ${PCRASTER_RASTERFORMAT_LIBRARY}
#         ${BOOST_FILESYSTEM_LIBRARY}
#         ${BOOST_SYSTEM_LIBRARY}
#     )
# ELSE(WIN32)
    TARGET_LINK_LIBRARIES(table
        api
        tab
        app
        pcrcom
        mathx
        misc
        ${PCRASTER_RASTER_FORMAT_LIBRARIES}
    )
    SET_TARGET_PROPERTIES(table PROPERTIES LINKER_LANGUAGE CXX)
# ENDIF(WIN32)

INSTALL(
    TARGETS table
    RUNTIME DESTINATION bin COMPONENT Runtime
)
