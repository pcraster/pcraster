add_executable(mapdiff
    mapdiff.cc
)

target_link_libraries(mapdiff
    pcraster::app
    pcraster::pcrgeo
    pcraster::raster_format
)
