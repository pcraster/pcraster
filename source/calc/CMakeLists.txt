include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
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
    mathx
    pcrcom
    tab
    geom
    misc
    # ${Boost_MATH_C99_LIBRARY}
)


if(FALSE AND PCRASTER_BUILD_TEST)
    SET(TEST_SOURCES
        calc_test.cc
        calc_downstreamvisitortest.cc
        calc_marktest.cc
        calc_kinematictest.cc
        calc_transienttest.cc
    )
    ADD_EXECUTABLE(test_calc
        ${TEST_SOURCES}
    )

    TARGET_LINK_LIBRARIES(test_calc
        calc
        # api
        # pcrgeo
        # app
        # mathx
        # pcrcom
        # tab
        # geom
        # misc
        # ${BOOST_SYSTEM_LIBRARY}
        # ${BOOST_FILESYSTEM_LIBRARY}
    )

    ADD_CUSTOM_TARGET(run_test_calc
        COMMAND test_calc --detect_memory_leaks=0
        DEPENDS test_calc
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    ADD_DEPENDENCIES(tests run_test_calc)
endif()
