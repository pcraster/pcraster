add_executable(asc2map
    arcgrid.c
    asc2map.c
    main.c
)

target_link_libraries(asc2map
    pcraster::app
    pcraster::misc
    pcraster::raster_format
)

INSTALL(
    TARGETS
        asc2map
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
)
