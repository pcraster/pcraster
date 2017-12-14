add_executable(legend
    main
    select
)

target_link_libraries(legend
    PRIVATE
        pcraster::app
        pcraster::curr
        pcraster::raster_format
)

### IF(WIN32)
###     SET(CMAKE_EXE_LINKER_FLAGS /NODEFAULTLIB:LIBCMT)
### ENDIF()

install(
    TARGETS legend
    RUNTIME
        DESTINATION bin COMPONENT Runtime
)
