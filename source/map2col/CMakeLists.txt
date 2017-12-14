add_executable(map2col
    map2col
    main
)

target_link_libraries(map2col
    PRIVATE
        pcraster::app
        pcraster::raster_format
)

install(
    TARGETS map2col
    RUNTIME
        DESTINATION bin COMPONENT Runtime
)
