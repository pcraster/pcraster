# geo_csfraster.cc
# geo_griddedpoints.cc
# geo_kernel.cc
# geo_kernelengine.cc
# geo_kernelsourcebuffer.cc
# geo_raster.cc

add_library(pcrgeo STATIC
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

add_library(pcraster::pcrgeo ALIAS pcrgeo)

target_include_directories(pcrgeo
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/.>
)

target_link_libraries(pcrgeo
    pcraster::app
    pcraster::dal
    pcraster::pcrcom
)

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    add_test(
        NAME setup_pcrgeo
        COMMAND ${Python_EXECUTABLE} test_prolog.py
    )

    add_unit_tests(
        SCOPE pcrgeo
        NAMES
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
            geo_utiltest.cc
        LINK_LIBRARIES
            pcraster::pcrgeo
    )

    set_tests_properties(setup_pcrgeo
        PROPERTIES
            FIXTURES_SETUP
                FixturePcrGeo
    )

    set_tests_properties(pcrgeo_geo_addfiltertest
        PROPERTIES
            FIXTURES_REQUIRED
                FixturePcrGeo
    )
endif()
