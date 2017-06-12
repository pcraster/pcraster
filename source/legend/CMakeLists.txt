include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../curr
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
)

SET(SOURCES
    main.c
    select.c
)

ADD_EXECUTABLE(legend
    ${SOURCES}
)

IF(WIN32)
    SET(CMAKE_EXE_LINKER_FLAGS /NODEFAULTLIB:LIBCMT)
ENDIF()
#     TARGET_LINK_LIBRARIES(legend
#         app
#         pcrcom
#         misc
#         curr
#         ${PCRASTER_RASTERFORMAT_LIBRARY}
#         ${PDCURSES_LIBRARIES}
#         ${BOOST_FILESYSTEM_LIBRARY}
#         ${BOOST_SYSTEM_LIBRARY}
#     )
# ELSE(WIN32)
    TARGET_LINK_LIBRARIES(legend
        # ncurses
        app
        # pcrcom
        # misc
        curr
        pcraster_raster_format
        ${CURSES_LIBRARIES}
    )
    SET_TARGET_PROPERTIES(legend PROPERTIES LINKER_LANGUAGE CXX)
# ENDIF(WIN32)

INSTALL(
    TARGETS legend
    RUNTIME DESTINATION bin COMPONENT Runtime
)
