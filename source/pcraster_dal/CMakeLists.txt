CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/dal_Configure.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/../include/dal_Configure.h
)
CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/Makefile.testrun.in
    ${CMAKE_CURRENT_BINARY_DIR}/Makefile.testrun
    @ONLY
)
CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/createTemporalVectorData.sh.in
    ${CMAKE_CURRENT_BINARY_DIR}/createTemporalVectorData.sh
    @ONLY
)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../utils
)

# SET(LINK_LIBRARIES
#     ${PCRASTER_RASTERFORMAT_LIBRARY}
#     ${DEVENV_UTILS_LIBRARY}
#     ${CURL_LIBRARIES}
#     ${GDAL_LIBRARIES}
#     ${QT_QTSQL_LIBRARY}
#     ${QT_QTCORE_LIBRARY}
#     ${BOOST_FILESYSTEM_LIBRARY}
#     ${BOOST_DATE_TIME_LIBRARY}
#     ${BOOST_REGEX_LIBRARY}
#     ${BOOST_SYSTEM_LIBRARY}
# )

SET(SOURCES
    dal_BasicTypes.cc
    dal_BinaryTableDriver.cc
    dal_Block.cc
    dal_BlockDriver.cc
    dal_BrowseInfo.cc
    dal_Client.cc
    dal_ConnectionInfo.cc
    dal_Constant.cc
    dal_ConstantDriver.cc
    dal_CoordinateMapper.cc
    dal_CSFMap.cc
    dal_CSFRasterDriver.cc
    dal_Dal.cc
    dal_Dataset.cc
    dal_DataSource.cc
    dal_DataSpaceAddress.cc
    dal_DataSpaceAddressMapper.cc
    dal_DataSpace.cc
    dal_DataSpaceIterator.cc
    dal_DataSpaceQueryResult.cc
    dal_Dimension.cc
    dal_Driver.cc
    dal_Environment.cc
    dal_FeatureDriver.cc
    dal_FeatureLayer.cc
    dal_FeatureLayerGeometries.cc
    dal_FeaturePath.cc
    dal_FilesystemUtils.cc
    dal_Format.cc
    dal_Formats.cc
    dal_GDALRasterDriver.cc
    dal_GeoEASTableDriver.cc
    dal_GSLIBBlockDriver.cc
    # dal_HDF5RasterDriver.cc
    dal_Library.cc
    dal_MapperUtils.cc
    dal_MathUtils.cc
    dal_Matrix.cc
    dal_MatrixDal.cc
    dal_MatrixDimensions.cc
    dal_MatrixDriver.cc
    dal_MemoryData.cc
    dal_MemoryDataPool.cc
    dal_MemoryRasterData.cc
    dal_MemoryRasterDriver.cc
    dal_MemoryTableData.cc
    dal_MemoryTableDriver.cc
    # dal_NetCDFRasterDriver.cc
    dal_OgrFeatureDriver.cc
    dal_PCRBlockDriver.cc
    dal_Properties.cc
    dal_Raster.cc
    dal_RasterDal.cc
    dal_RasterDriver.cc
    dal_RasterDimensions.cc
    dal_RegularExpressions.cc
    dal_SpaceDimensions.cc
    dal_SpaceStepMapper.cc
    dal_SpaceStepCoordinateMapper.cc
    dal_SpatialCoordinate.cc
    dal_SQLTableDriver.cc
    dal_StackInfo.cc
    dal_StepCoordinateMapper.cc
    dal_StepMapper.cc
    dal_TableDal.cc
    dal_TableDriver.cc
    dal_TextConstantDriver.cc
    dal_TextFileDriver.cc
    dal_TextMatrixDriver.cc
    dal_TextTableDriver.cc
    dal_TimeStepCoordinateMapper.cc
    dal_TimeStepMapper.cc
    dal_Type.cc
    dal_Utils.cc
    dal_Vector.cc
    dal_VectorDriver.cc
    dal_VTKBlockDriver.cc
)
# IF(WIN32)
#     ADD_LIBRARY(pcraster_dal_static
#         STATIC
#         ${SOURCES}
#     )
#     SET_TARGET_PROPERTIES(pcraster_dal_static
#         PROPERTIES
#             COMPILE_FLAGS "-DPCR_DAL_SOURCE"
#     )
# ENDIF()
ADD_LIBRARY(pcraster_dal
    SHARED
    ${SOURCES}
)
SET_TARGET_PROPERTIES(pcraster_dal
    PROPERTIES
        COMPILE_FLAGS "-DPCR_DAL_SOURCE -DPCR_DAL_SHARED_LINK"
)
TARGET_LINK_LIBRARIES(pcraster_dal
    utils
    ${GDAL_LIBRARIES}
    ${PCRASTER_RASTER_FORMAT_LIBRARIES}
    ${Boost_DATE_TIME_LIBRARY}
    #${Boost_FILESYSTEM_LIBRARY}
    ${Boost_PROGRAM_OPTIONS_LIBRARY}
    ${Boost_SYSTEM_LIBRARY}
    Qt5::Sql
)
ADD_CUSTOM_COMMAND(
    TARGET pcraster_dal
    POST_BUILD
        COMMAND ${CMAKE_COMMAND}
            -E make_directory ${CMAKE_BINARY_DIR}/share/gdal
        COMMAND ${CMAKE_COMMAND}
            -E copy_directory ${GDAL_DATA} ${CMAKE_BINARY_DIR}/share/gdal
    COMMENT "Copying GDAL shared data stuff share/gdal"
)
INSTALL(
    DIRECTORY ${CMAKE_BINARY_DIR}/share/gdal
    DESTINATION "share"
)
# ------------------------------------------------------------------------------

if(PCRASTER_BUILD_TEST)
    configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/test_prolog.py.in
        ${CMAKE_CURRENT_BINARY_DIR}/test_prolog.py
        @ONLY
    )
    set(TEST_NAMES
        dal_ArrayTest
        dal_BinaryTableDriverTest
        dal_BlockTest
        dal_ClientTest
        dal_ConnectionInfoTest
        dal_ConstantTest
        dal_CoordinateMapperTest
        dal_CSFMapTest
        dal_CSFRasterDriverTest
        dal_DataSourceTest
        # dal_DataSpaceAddressMapperTest  # TODO
        dal_DataSpaceAddressTest
        dal_DataSpaceIteratorTest
        dal_DataSpaceTest
        dal_DefTest
        dal_DimensionTest
        dal_EnvironmentTest
        dal_FeatureLayerTest
        dal_FeaturePathTest
        dal_FilesystemUtilsTest
        dal_GDALRasterDriverTest
        dal_GeoEASTableDriverTest
        # dal_HDF5RasterDriverTest
        # dal_MapperUtilsTest  # TODO
        dal_MathUtilsTest
        dal_MatrixTest
        dal_MatrixDimensionsTest
        dal_MatrixDriverTest
        dal_MemoryDataPoolTest
        dal_MemoryRasterDataTest
        dal_MemoryRasterDriverTest
        dal_MemoryTableDataTest
        dal_MemoryTableDriverTest
        # dal_NetCDFRasterDriverTest
        dal_OgrFeatureDriverTest
        dal_PCRBlockDriverTest
        dal_PropertiesTest
        dal_RasterDalTest
        dal_RasterDimensionsTest
        dal_RegularExpressionsTest
        dal_ResampleTest
        dal_SpaceStepMapperTest
        # dal_SpaceStepCoordinateMapperTest  # TODO
        dal_SpatialCoordinateTest
        # dal_SQLTableDriverTest  # TODO
        dal_StackInfoTest
        # dal_StepCoordinateMapperTest  # TODO
        dal_StepMapperTest
        dal_TableDalTest
        dal_TableDriverTest
        dal_TableTest
        dal_TextConstantDriverTest
        dal_TextFileDriverTest
        dal_TextMatrixDriverTest
        dal_TextTableDriverTest
        dal_TimeStepCoordinateMapperTest
        dal_TimeStepMapperTest
        dal_TypesTest
        dal_TypeTest
        dal_UseCasesTest
        dal_UtilsTest
        dal_VectorDriverTest
        dal_VTKBlockDriverTest
    )
    add_unit_tests(
        SCOPE pcraster_dal
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            pcraster_dal
    )
    add_test(
        NAME setup_pcraster_dal
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )
    set_tests_properties(setup_pcraster_dal
        PROPERTIES FIXTURES_SETUP FixturePCRasterDal
    )
    set_tests_properties(pcraster_dal_dal_ArrayTest PROPERTIES
        FIXTURES_REQUIRED
          FixturePCRasterDal
    )
endif()
# ------------------------------------------------------------------------------
SET(DALAPP_SOURCES
    dal_Application.cc
    main.cc
)
ADD_EXECUTABLE(dal
    ${DALAPP_SOURCES}
)
SET_TARGET_PROPERTIES(dal
    PROPERTIES
        COMPILE_FLAGS "-DPCR_DAL_SHARED_LINK"
)
TARGET_LINK_LIBRARIES(dal
    pcraster_dal
)
# ------------------------------------------------------------------------------
# Install rules for all components.
INSTALL(
    TARGETS pcraster_dal
    RUNTIME DESTINATION lib COMPONENT Runtime
    LIBRARY DESTINATION lib COMPONENT Runtime
)
