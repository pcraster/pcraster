add_library(calc STATIC
    accu.c
    accufrac.c
    accultc.c
    accutrig.c
    accutt.c
    area.c
    birds.c
    calc_downstreamvisitor.cc
    calc_extentofview.cc
    calc_neighbourhood.cc
    catch.c
    catcstat.c
    clump.c
    curv.c
    delta.c
    diffuse.c
    distributesimplegauss.cc
    doaccu.c
    downstreamtotal.c
    drain.c
    dwnstrm.c
    dynwave.cc
    ellipse.c
    horizonangle.c
    ibngauss.c
    idi.cc
    influencesimplegauss.cc
    kinemati.c
    ldd.c
    ldddist.c
    lddlist.c
    lddm.c
    lddmnd.c
    list.c
    mark.cc
    move.c
    order.c
    orient.c
    path.c
    pitrem.c
    pitremnd.c
    repair.c
    slope.c
    slopelen.c
    sprdldd.c
    sprdmax.c
    spread.c
    strorder.c
    summary.c
    timein.c
    calc_transient.cc
    tt.c
    upstream.c
    view.c
    window.c
    vf.c
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
    PRIVATE
        Boost::headers
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE calc
        NAMES
            calc_downstreamvisitortest.cc
            calc_marktest.cc
            calc_kinematictest.cc
            calc_transienttest.cc
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
