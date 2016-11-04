include_directories(
    # ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../curr
    ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
)

SET(SOURCES
    mboth.c
    mclone.c
    main.c
)

ADD_EXECUTABLE(mapattr
    ${SOURCES}
)

IF(WIN32)
    SET(CMAKE_EXE_LINKER_FLAGS /NODEFAULTLIB:LIBCMT)
ENDIF()
    # TARGET_LINK_LIBRARIES(mapattr
    #     mathx
    #     app
    #     pcrcom
    #     misc
    #     curr
    #     ${PCRASTER_RASTERFORMAT_LIBRARY}
    #     ${PDCURSES_LIBRARIES}
    #     ${BOOST_FILESYSTEM_LIBRARY}
    #     ${BOOST_SYSTEM_LIBRARY}
    # )
# ELSE(WIN32)
    TARGET_LINK_LIBRARIES(mapattr
        mathx
        app
        # pcrcom
        # misc
        curr
        ${PCRASTER_RASTER_FORMAT_LIBRARIES}
        ${CURSES_LIBRARIES}
    )
    SET_TARGET_PROPERTIES(mapattr PROPERTIES LINKER_LANGUAGE CXX)
# ENDIF(WIN32)


INSTALL(
    TARGETS mapattr
    RUNTIME DESTINATION bin COMPONENT Runtime
)
