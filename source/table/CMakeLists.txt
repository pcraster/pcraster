add_executable(table
    main.c
    move.c
)

target_link_libraries(table
    PRIVATE
        pcraster::app
        pcraster::raster_format
        pcraster::tab
)

install(
    TARGETS table
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
)
