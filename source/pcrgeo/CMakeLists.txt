CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
)

include_directories(
    # ${CMAKE_CURRENT_SOURCE_DIR}/../include
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
    pcrcom
    pcraster_dal
    ${PCRASTER_RASTER_FORMAT_LIBRARIES}
    ${Boost_MATH_C99_LIBRARY}
)


if(PCRASTER_BUILD_TEST)
    SET(TEST_NAMES
        geo_addfiltertest
        geo_algorithmtest
        geo_assignfiltertest
        geo_averagefiltertest
        geo_bandmaptest
        geo_celllocvisitortest
        geo_circularneighbourhoodtest
        geo_countfiltertest
        geo_csfmaptest
        geo_csfstacknametest
        geo_filecreatetestertest
        geo_fractionfiltertest
        geo_griddedpointstest
        geo_mooreneighbourhoodtest
        geo_neighbourhoodtest
        geo_neighbourtest
        geo_pointtest
        geo_pointvaluetest
        geo_rasterboundariestest
        geo_rasterdimtest
        geo_rasterfileconvertertest
        geo_rasterspacetest
        geo_rastertest
        geo_riksneighbourhoodtest
        geo_scanconversiontest
        geo_simplerastertest
        geo_squaretest
        geo_utiltest
    )
    add_unit_tests(
        SCOPE pcrgeo
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            pcrgeo
            pcrcom
            pcraster_dal
    )
endif()
