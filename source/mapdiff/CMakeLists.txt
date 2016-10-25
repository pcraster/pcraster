if(PCRASTER_BUILD_TEST)
    include_directories(
        ${CMAKE_CURRENT_SOURCE_DIR}/../app
        ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
        ${CMAKE_CURRENT_SOURCE_DIR}/../misc
        ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
        ${CMAKE_CURRENT_SOURCE_DIR}/../pcrgeo
    )

    SET(SOURCES
        mapdiff.cc
    )

    ADD_EXECUTABLE(mapdiff
        ${SOURCES}
    )

    TARGET_LINK_LIBRARIES(mapdiff
        pcrgeo
        pcrcom
        app
        misc
        pcraster_dal
        ${PCRASTER_RASTER_FORMAT_LIBRARIES}
        # ${Boost_MATH_C99_LIBRARY}
    )
endif()
