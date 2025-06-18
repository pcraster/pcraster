add_library(app STATIC
    app.c
    appclone.c
    appcoord.c
    appcr.c
    appdir.c
    appinst.c
    # applog.c
    app_options.cc
    apptime.c
    apptxt.c
    appval.c
    # appvers.c
    aprowcol.c
    inpfile.c
    readcols.c
    $<$<BOOL:${WIN32}>:pcrshell.cc>
)

add_library(pcraster::app ALIAS app)

target_include_directories(app
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>/../include
)

target_link_libraries(app
    pcraster::mathx
    pcraster::pcrcom
    pcraster::raster_format
)
