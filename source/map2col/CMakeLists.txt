add_executable(map2col
    map2col.c
    main.c
)

target_link_libraries(map2col
    PRIVATE
        pcraster::app
        pcraster::raster_format
)

install(
    TARGETS map2col
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
)
