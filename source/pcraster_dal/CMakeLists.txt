CONFIGURE_FILE(
    Makefile.testrun.in
    Makefile.testrun
    @ONLY
)
CONFIGURE_FILE(
    createTemporalVectorData.sh.in
    createTemporalVectorData.sh
    @ONLY
)

add_library(pcraster_dal_objects OBJECT
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

target_include_directories(pcraster_dal_objects SYSTEM
    PUBLIC
        ${GDAL_INCLUDE_DIRS}
        ${Qt5Sql_INCLUDE_DIRS}
    PRIVATE
        ${Boost_INCLUDE_DIRS}
)

target_include_directories(pcraster_dal_objects
    PRIVATE
        $<TARGET_PROPERTY:pcraster::raster_format,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::utils,INTERFACE_INCLUDE_DIRECTORIES>
)

target_include_directories(pcraster_dal_objects
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_compile_definitions(pcraster_dal_objects
    PRIVATE
        PCR_DAL_SOURCE
        PCR_DAL_SHARED_LINK
    INTERFACE
        PCR_DAL_SHARED_LINK
)

add_library(pcraster_dal SHARED
    $<TARGET_OBJECTS:pcraster_dal_objects>
)

target_compile_definitions(pcraster_dal
    PUBLIC
        PCR_DAL_SHARED_LINK
)

add_library(pcraster::dal ALIAS pcraster_dal)

set_target_properties(pcraster_dal
    PROPERTIES
        SOVERSION ${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}
)

target_include_directories(pcraster_dal
    PUBLIC
        $<TARGET_PROPERTY:pcraster_dal_objects,INTERFACE_INCLUDE_DIRECTORIES>
)

target_link_libraries(pcraster_dal
    PUBLIC
        pcraster::raster_format
        pcraster::utils
        Qt5::Sql
        GDAL::GDAL
        Boost::date_time
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
if(PCRASTER_INSTALL_FILES_GDAL)
    INSTALL(
        DIRECTORY ${CMAKE_BINARY_DIR}/share/gdal
        DESTINATION "share"
    )
endif()
# ------------------------------------------------------------------------------

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )
    set(DAL_TEST_SOURCES
        dal_ArrayTest.cc
        dal_BinaryTableDriverTest.cc
        dal_BlockTest.cc
        dal_ClientTest.cc
        dal_ConnectionInfoTest.cc
        dal_ConstantTest.cc
        dal_CoordinateMapperTest.cc
        dal_CSFMapTest.cc
        dal_CSFRasterDriverTest.cc
        dal_DataSourceTest.cc
        # dal_DataSpaceAddressMapperTest  # TODO
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
        # dal_MapperUtilsTest  # TODO
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
        # dal_SpaceStepCoordinateMapperTest  # TODO
        dal_SpatialCoordinateTest.cc
        # dal_SQLTableDriverTest  # TODO
        dal_StackInfoTest.cc
        # dal_StepCoordinateMapperTest  # TODO
        dal_StepMapperTest.cc
        dal_TableDalTest.cc
        dal_TableDriverTest.cc
        dal_TableTest.cc
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
    add_unit_tests(
        SCOPE pcraster_dal
        NAMES
            ${DAL_TEST_SOURCES}
        INCLUDE_DIRS
            $<TARGET_PROPERTY:pcraster_dal_objects,INTERFACE_INCLUDE_DIRECTORIES>
        OBJECT_LIBRARIES
            $<TARGET_OBJECTS:pcraster_dal_objects>
        LINK_LIBRARIES
            $<TARGET_PROPERTY:pcraster_dal,LINK_LIBRARIES>
    )
    set_property(
        SOURCE ${DAL_TEST_SOURCES}
        PROPERTY COMPILE_DEFINITIONS PCR_DAL_SHARED_LINK
    )
    add_test(
        NAME setup_pcraster_dal
        COMMAND ${Python3_EXECUTABLE} test_prolog.py
    )
    set_tests_properties(setup_pcraster_dal
        PROPERTIES FIXTURES_SETUP
            FixturePCRasterDal
    )
    set_tests_properties(
            pcraster_dal_dal_DataSourceTest.cc
            pcraster_dal_dal_DataSourceTest.cc
            pcraster_dal_dal_CSFMapTest.cc
            pcraster_dal_dal_CSFRasterDriverTest.cc
        PROPERTIES
            FIXTURES_REQUIRED
                FixturePCRasterDal
    )
endif()

# ------------------------------------------------------------------------------

# add_executable(dal
#     dal_Application.cc
#     main.cc
# )
#
# target_compile_definitions(dal
#     PRIVATE
#         PCR_DAL_SHARED_LINK
# )
#
# target_link_libraries(dal
#     Boost::program_options
#     pcraster::dal
# )

# ------------------------------------------------------------------------------

install(
    TARGETS
        pcraster_dal
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
    LIBRARY
        DESTINATION ${PCRASTER_LIB_INSTALL_DIR} COMPONENT Runtime
)
