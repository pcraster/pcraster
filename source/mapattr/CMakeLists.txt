add_executable(mapattr
    mboth.c
    mclone.c
    main.c
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
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
)
