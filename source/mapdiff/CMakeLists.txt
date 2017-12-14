add_executable(mapdiff
    mapdiff
)

target_link_libraries(mapdiff
    pcraster::app
    pcraster::pcrgeo
    pcraster::raster_format
)
