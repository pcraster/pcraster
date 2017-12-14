add_library(calc STATIC
    accu
    accufrac
    accultc
    accutrig
    accutt
    area
    birds
    calc_downstreamvisitor.cc
    calc_extentofview.cc
    calc_neighbourhood.cc
    catch
    catcstat
    clump
    curv
    delta
    diffuse
    distributesimplegauss.cc
    doaccu
    downstreamtotal
    drain
    dwnstrm
    dynwave.cc
    ellipse
    horizonangle
    ibngauss
    idi.cc
    influencesimplegauss.cc
    kinemati
    ldd
    ldddist
    lddlist
    lddm
    lddmnd
    list
    mark.cc
    move
    order
    orient
    path
    pitrem
    pitremnd
    repair
    slope
    slopelen
    sprdldd
    sprdmax
    spread
    strorder
    summary
    timein
    calc_transient.cc
    tt
    upstream
    view
    window
    vf
)

add_library(pcraster::calc ALIAS calc)

target_include_directories(calc
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(calc
    PUBLIC
        pcraster::api
        pcraster::app
        pcraster::geom
        pcraster::tab
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE calc
        NAMES
            calc_downstreamvisitortest
            calc_marktest
            calc_kinematictest
            calc_transienttest
        LINK_LIBRARIES
            pcraster::calc
    )
endif()


# /*!
#    \namespace calc
#    \brief global field functions of pcrcalc
#
#    Calc is a bad name for the namespace;
#    In the long run, merge this lib and api, to be named globfieldfunc ??
# */
