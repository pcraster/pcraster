add_library(app STATIC
    app
    appclone
    appcoord
    appcr
    appdir
    appinst
    applog
    app_options
    apptime
    apptxt
    appval
    appvers
    aprowcol
    inpfile
    readcols
    $<$<BOOL:${WIN32}>:pcrshell>
)

add_library(pcraster::app ALIAS app)

target_include_directories(app
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(app
    pcraster::mathx
    pcraster::pcrcom
    pcraster::raster_format
)
