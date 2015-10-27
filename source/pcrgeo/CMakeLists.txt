CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
)

# geo_csfraster.cc
# geo_griddedpoints.cc
# geo_kernel.cc
# geo_kernelengine.cc
# geo_kernelsourcebuffer.cc
# geo_raster.cc
SET(SOURCES
    geo_addfilter.cc
    geo_apprasterspace.cc
    geo_assignfilter.cc
    geo_averagefilter.cc
    geo_bandmap.cc
    geo_block.cc
    geo_cellloc.cc
    geo_celllocvisitor.cc
    geo_circularneighbourhood.cc
    geo_columnfile.cc
    geo_const.cc
    geo_csfmap.cc
    # geo_csfraster.cc
    geo_csfstack.cc
    geo_csfstackname.cc
    geo_exception.cc
    geo_filecreatetester.cc
    # geo_griddedpoints.cc
    geo_idi.cc
    # geo_kernel.cc
    # geo_kernelengine.cc
    # geo_kernelsourcebuffer.cc
    geo_mooreneighbourhood.cc
    geo_neighbour.cc
    geo_neighbourhood.cc
    geo_point.cc
    # geo_raster.cc
    geo_rasterfileconverter.cc
    geo_rasterspace.cc
    geo_rectangle.cc
    geo_riksneighbourhood.cc
    geo_samecellrepr.cc
    geo_templates.cc
    geo_util.cc
    geo_voxel.cc
    geo_voxelstack.cc
    geo_wmatrix.cc
)
ADD_LIBRARY(pcrgeo
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(pcrgeo
    ${PCRASTER_RASTER_FORMAT_LIBRARIES}
    ${Boost_MATH_C99_LIBRARY}
)


if(FALSE AND PCRASTER_BUILD_TEST)
    SET(TEST_SOURCES
        geo_addfiltertest.cc
        geo_algorithmtest.cc
        geo_assignfiltertest.cc
        geo_averagefiltertest.cc
        geo_bandmaptest.cc
        geo_celllocvisitortest.cc
        geo_circularneighbourhoodtest.cc
        geo_countfiltertest.cc
        geo_csfmaptest.cc
        geo_csfstacknametest.cc
        geo_filecreatetestertest.cc
        geo_fractionfiltertest.cc
        geo_griddedpointstest.cc
        geo_mooreneighbourhoodtest.cc
        geo_neighbourhoodtest.cc
        geo_neighbourtest.cc
        geo_pointtest.cc
        geo_pointvaluetest.cc
        geo_rasterboundariestest.cc
        geo_rasterdimtest.cc
        geo_rasterfileconvertertest.cc
        geo_rasterspacetest.cc
        geo_rastertest.cc
        geo_riksneighbourhoodtest.cc
        geo_scanconversiontest.cc
        geo_simplerastertest.cc
        geo_squaretest.cc
        geo_test.cc
        geo_utiltest.cc
    )
    ADD_EXECUTABLE(test_pcrgeo
        ${TEST_SOURCES}
    )
    TARGET_LINK_LIBRARIES(test_pcrgeo
        pcrgeo
        pcrcom
        pcraster_dal
    )
    ADD_CUSTOM_TARGET(run_test_pcrgeo
        COMMAND bash ./testrun.prolog
        COMMAND test_pcrgeo
        DEPENDS test_pcrgeo
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    ADD_DEPENDENCIES(tests run_test_pcrgeo)
endif()
