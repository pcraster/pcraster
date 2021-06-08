add_executable(map2asc
    map2asc.c
    main.c
)

target_link_libraries(map2asc
    PRIVATE
        pcraster::app
        pcraster::raster_format
)

install(
      TARGETS map2asc
      RUNTIME
          DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
)
