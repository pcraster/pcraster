add_executable(asc2map
    arcgrid
    asc2map
    main
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
        DESTINATION bin COMPONENT Runtime
)
