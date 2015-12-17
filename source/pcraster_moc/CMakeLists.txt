# TODO Create Documentation, PCRCalc, Python, ... subdirectories.
ADD_SUBDIRECTORY(python)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_old_calc
)

SET(SOURCES
    pt_MOCLink.cc
    pt_ModelLinks.cc
    pt_Particle.cc
    pt_ParticleTracker.cc
)
SET(TARGET_LINK_LIBS
    pcrgeo
    pcrcom
    mathx
    misc
    pcraster_dal
    # ${PCRASTER_DAL_LIBRARY}
    # ${PCRASTER_RASTERFORMAT_LIBRARY}
    # ${BOOST_SYSTEM_LIBRARY}
    # ${BOOST_FILESYSTEM_LIBRARY}
    # ${Boost_MATH_C99_LIBRARY}
)
ADD_LIBRARY(pcraster_moc
    SHARED
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(pcraster_moc
    ${TARGET_LINK_LIBS}
)


ADD_LIBRARY(pcraster_moc_static
    STATIC
    ${SOURCES}
)


INSTALL(
    TARGETS pcraster_moc
    RUNTIME DESTINATION lib COMPONENT Runtime
    LIBRARY DESTINATION lib COMPONENT Runtime
)


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        pt_ParticleTest
    )

    add_unit_tests(
        SCOPE moc
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            pcraster_moc
    )
endif()


# run:
# 	runapp pcrcalc -f track.pcr
#
# debug.map: out dem.map
# 	runapp col2map --unitcell --clone dem.map -B out debug.map
#
# flux.map: flux.tbl dem.map
# 	runapp pcrcalc "flux.map=lookupscalar(flux.tbl, dem.map)"
# 	runapp pcrcalc "flux.map=if(defined(dem.map) and not(defined(flux.map)), 0.0, flux.map)"
