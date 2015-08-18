CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/dal_Configure.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/../include/dal_Configure.h
)
CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
)
CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.epilog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.epilog
    @ONLY
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
#     ${GEOS_LIBRARIES}
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
    ${GEOS_LIBRARY}
    ${PCRASTER_RASTER_FORMAT_LIBRARIES}
    ${Boost_REGEX_LIBRARY}
    ${Boost_DATE_TIME_LIBRARY}
    ${Boost_FILESYSTEM_LIBRARY}
    ${Boost_PROGRAM_OPTIONS_LIBRARY}
    ${Boost_SYSTEM_LIBRARY}
    Qt4::QtSql
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
SET(TEST_SOURCES
    dal_ArrayTest.cc
    dal_BinaryTableDriverTest.cc
    dal_BlockTest.cc
    dal_ClientTest.cc
    dal_ConnectionInfoTest.cc
    dal_ConstantTest.cc
    dal_CoordinateMapperTest.cc
    dal_CSFMapTest.cc
    dal_CSFRasterDriverTest.cc
    dal_DalTest.cc
    dal_DataSourceTest.cc
    dal_DataSpaceAddressMapperTest.cc
    dal_DataSpaceAddressTest.cc
    dal_DataSpaceIteratorTest.cc
    dal_DataSpaceTest.cc
    dal_DefTest.cc
    dal_DimensionTest.cc
    dal_EnvironmentTest.cc
    dal_FeatureLayerTest.cc
    dal_FeaturePathTest.cc
    dal_FilesystemUtilsTest.cc
    dal_GDALRasterDriverTest.cc
    dal_GeoEASTableDriverTest.cc
    # dal_HDF5RasterDriverTest.cc
    dal_MapperUtilsTest.cc
    dal_MathUtilsTest.cc
    dal_MatrixTest.cc
    dal_MatrixDimensionsTest.cc
    dal_MatrixDriverTest.cc
    dal_MemoryDataPoolTest.cc
    dal_MemoryRasterDataTest.cc
    dal_MemoryRasterDriverTest.cc
    dal_MemoryTableDataTest.cc
    dal_MemoryTableDriverTest.cc
    # dal_NetCDFRasterDriverTest.cc
    dal_OgrFeatureDriverTest.cc
    dal_PCRBlockDriverTest.cc
    dal_PropertiesTest.cc
    dal_RasterDalTest.cc
    dal_RasterDimensionsTest.cc
    dal_RegularExpressionsTest.cc
    dal_ResampleTest.cc
    dal_SpaceStepMapperTest.cc
    dal_SpaceStepCoordinateMapperTest.cc
    dal_SpatialCoordinateTest.cc
    dal_SQLTableDriverTest.cc
    dal_StackInfoTest.cc
    dal_StepCoordinateMapperTest.cc
    dal_StepMapperTest.cc
    dal_TableDalTest.cc
    dal_TableDriverTest.cc
    dal_TableTest.cc
    dal_Test.cc
    dal_TextConstantDriverTest.cc
    dal_TextFileDriverTest.cc
    dal_TextMatrixDriverTest.cc
    dal_TextTableDriverTest.cc
    dal_TimeStepCoordinateMapperTest.cc
    dal_TimeStepMapperTest.cc
    dal_TypesTest.cc
    dal_TypeTest.cc
    dal_UseCasesTest.cc
    dal_UtilsTest.cc
    dal_VectorDriverTest.cc
    dal_VTKBlockDriverTest.cc
)
ADD_EXECUTABLE(test_pcraster_dal
    ${TEST_SOURCES}
)
# IF(WIN32)
#     TARGET_LINK_LIBRARIES(test_pcraster_dal
#         pcraster_dal_static
#     )
# ELSE()
    TARGET_LINK_LIBRARIES(test_pcraster_dal
        pcraster_dal
    )
# ENDIF()
### TARGET_LINK_LIBRARIES(test_pcraster_dal
###     # ${LINK_LIBRARIES}
### )

# TODO Bug in d83.map test? Legend detection seems wrong.
ADD_CUSTOM_TARGET(run_test_pcraster_dal
    COMMAND ${CMAKE_COMMAND} -E env
        LD_LIBRARY_PATH=${GDAL_INCLUDE_DIR}/../lib:$ENV{LD_LIBRARY_PATH}
        bash ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    COMMAND test_pcraster_dal --detect_memory_leaks=0
    # COMMAND vcexpress.exe /debugexe test_pcraster_dal
    COMMAND bash ${CMAKE_CURRENT_BINARY_DIR}/testrun.epilog
    DEPENDS test_pcraster_dal
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
# TODO Uncomment!!! ADD_DEPENDENCIES(tests run_test_pcraster_dal)
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
    # ${BOOST_PROGRAM_OPTIONS_LIBRARY}
)
# ------------------------------------------------------------------------------
# Install rules for all components.
INSTALL(
    TARGETS pcraster_dal
    RUNTIME DESTINATION lib COMPONENT Runtime
    LIBRARY DESTINATION lib COMPONENT Runtime
)
