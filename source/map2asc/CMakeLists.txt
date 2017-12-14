add_executable(map2asc
    map2asc
    main
)

target_link_libraries(map2asc
    PRIVATE
        pcraster::app
        pcraster::raster_format
)

install(
      TARGETS map2asc
      RUNTIME
          DESTINATION bin COMPONENT Runtime
)
