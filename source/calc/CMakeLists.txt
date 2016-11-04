include_directories(
    # ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../api
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../tab
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
    ${CMAKE_CURRENT_SOURCE_DIR}/../geom
)

SET(SOURCES
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
ADD_LIBRARY(calc
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(calc
    api
    pcrgeo
    app
    # mathx
    # pcrcom
    tab
    geom
    # misc
    # ${Boost_MATH_C99_LIBRARY}
)


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        calc_downstreamvisitortest
        calc_marktest
        calc_kinematictest
        calc_transienttest
    )
    add_unit_tests(
        SCOPE calc
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            calc
    )
endif()


# /*!
#    \namespace calc
#    \brief global field functions of pcrcalc
#
#    Calc is a bad name for the namespace;
#    In the long run, merge this lib and api, to be named globfieldfunc ??
# */
