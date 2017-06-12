include_directories(
    # ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
)

SET(SOURCES
    map2asc.c
    main.c
)

ADD_EXECUTABLE(map2asc
    ${SOURCES}
)

# IF(WIN32)
#     TARGET_LINK_LIBRARIES(map2asc
#         app
#         pcrcom
#         mathx
#         misc
#         ${PCRASTER_RASTERFORMAT_LIBRARY}
#         ${BOOST_FILESYSTEM_LIBRARY}
#         ${BOOST_SYSTEM_LIBRARY}
#     )
# ELSE(WIN32)
    TARGET_LINK_LIBRARIES(map2asc
        app
        # pcrcom
        # mathx
        misc
        pcraster_raster_format
    )
    SET_TARGET_PROPERTIES(map2asc PROPERTIES LINKER_LANGUAGE CXX)
# ENDIF(WIN32)

INSTALL(
      TARGETS map2asc
      RUNTIME DESTINATION bin COMPONENT Runtime
)
