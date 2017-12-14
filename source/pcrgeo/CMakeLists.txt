# geo_csfraster.cc
# geo_griddedpoints.cc
# geo_kernel.cc
# geo_kernelengine.cc
# geo_kernelsourcebuffer.cc
# geo_raster.cc

add_library(pcrgeo STATIC
    geo_addfilter
    geo_apprasterspace
    geo_assignfilter
    geo_averagefilter
    geo_bandmap
    geo_block
    geo_cellloc
    geo_celllocvisitor
    geo_circularneighbourhood
    geo_columnfile
    geo_const
    geo_csfmap
    # geo_csfraster
    geo_csfstack
    geo_csfstackname
    geo_exception
    geo_filecreatetester
    # geo_griddedpoints
    geo_idi
    # geo_kernel
    # geo_kernelengine
    # geo_kernelsourcebuffer
    geo_mooreneighbourhood
    geo_neighbour
    geo_neighbourhood
    geo_point
    # geo_raster
    geo_rasterfileconverter
    geo_rasterspace
    geo_rectangle
    geo_riksneighbourhood
    geo_samecellrepr
    geo_templates
    geo_util
    geo_voxel
    geo_voxelstack
    geo_wmatrix
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
    Boost::math_c99
)

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    add_test(
        NAME setup_pcrgeo
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )

    add_unit_tests(
        SCOPE pcrgeo
        NAMES
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
