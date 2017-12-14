add_executable(mapattr
    mboth
    mclone
    main
)

target_link_libraries(mapattr
    PRIVATE
        pcraster::app
        pcraster::curr
        pcraster::raster_format
)

### IF(WIN32)
###     SET(CMAKE_EXE_LINKER_FLAGS /NODEFAULTLIB:LIBCMT)
### ENDIF()

install(
    TARGETS mapattr
    RUNTIME
        DESTINATION bin COMPONENT Runtime
)
