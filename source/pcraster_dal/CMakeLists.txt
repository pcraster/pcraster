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
    dal_BasicTypes
    dal_BinaryTableDriver
    dal_Block
    dal_BlockDriver
    dal_BrowseInfo
    dal_Client
    dal_ConnectionInfo
    dal_Constant
    dal_ConstantDriver
    dal_CoordinateMapper
    dal_CSFMap
    dal_CSFRasterDriver
    dal_Dal
    dal_Dataset
    dal_DataSource
    dal_DataSpaceAddress
    dal_DataSpaceAddressMapper
    dal_DataSpace
    dal_DataSpaceIterator
    dal_DataSpaceQueryResult
    dal_Dimension
    dal_Driver
    dal_Environment
    dal_FeatureDriver
    dal_FeatureLayer
    dal_FeatureLayerGeometries
    dal_FeaturePath
    dal_FilesystemUtils
    dal_Format
    dal_Formats
    dal_GDALRasterDriver
    dal_GeoEASTableDriver
    dal_GSLIBBlockDriver
    # dal_HDF5RasterDriver
    dal_Library
    dal_MapperUtils
    dal_MathUtils
    dal_Matrix
    dal_MatrixDal
    dal_MatrixDimensions
    dal_MatrixDriver
    dal_MemoryData
    dal_MemoryDataPool
    dal_MemoryRasterData
    dal_MemoryRasterDriver
    dal_MemoryTableData
    dal_MemoryTableDriver
    # dal_NetCDFRasterDriver
    dal_OgrFeatureDriver
    dal_PCRBlockDriver
    dal_Properties
    dal_Raster
    dal_RasterDal
    dal_RasterDriver
    dal_RasterDimensions
    dal_RegularExpressions
    dal_SpaceDimensions
    dal_SpaceStepMapper
    dal_SpaceStepCoordinateMapper
    dal_SpatialCoordinate
    dal_SQLTableDriver
    dal_StackInfo
    dal_StepCoordinateMapper
    dal_StepMapper
    dal_TableDal
    dal_TableDriver
    dal_TextConstantDriver
    dal_TextFileDriver
    dal_TextMatrixDriver
    dal_TextTableDriver
    dal_TimeStepCoordinateMapper
    dal_TimeStepMapper
    dal_Type
    dal_Utils
    dal_Vector
    dal_VectorDriver
    dal_VTKBlockDriver
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
)

add_library(pcraster_dal SHARED
    $<TARGET_OBJECTS:pcraster_dal_objects>
)

add_library(pcraster::dal ALIAS pcraster_dal)

target_include_directories(pcraster_dal
    PUBLIC
        $<TARGET_PROPERTY:pcraster_dal_objects,INTERFACE_INCLUDE_DIRECTORIES>
)

target_link_libraries(pcraster_dal
    PUBLIC
        pcraster::raster_format
        pcraster::utils
        Qt5::Sql
        ${GDAL_LIBRARIES}
        Boost::filesystem
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
INSTALL(
    DIRECTORY ${CMAKE_BINARY_DIR}/share/gdal
    DESTINATION "share"
)

# ------------------------------------------------------------------------------

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    add_unit_tests(
        SCOPE pcraster_dal
        NAMES
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
        INCLUDE_DIRS
            $<TARGET_PROPERTY:pcraster_dal_objects,INTERFACE_INCLUDE_DIRECTORIES>
        OBJECT_LIBRARIES
            $<TARGET_OBJECTS:pcraster_dal_objects>
        LINK_LIBRARIES
            $<TARGET_PROPERTY:pcraster_dal,LINK_LIBRARIES>
    )
    add_test(
        NAME setup_pcraster_dal
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )
    set_tests_properties(setup_pcraster_dal
        PROPERTIES FIXTURES_SETUP
            FixturePCRasterDal
    )
    set_tests_properties(
            pcraster_dal_dal_ArrayTest
            pcraster_dal_dal_DataSourceTest
        PROPERTIES
            FIXTURES_REQUIRED
                FixturePCRasterDal
    )
endif()

# ------------------------------------------------------------------------------

add_executable(dal
    dal_Application.cc
    main.cc
)

target_compile_definitions(dal
    PRIVATE
        PCR_DAL_SHARED_LINK
)

target_link_libraries(dal
    Boost::program_options
    pcraster::dal
)

# ------------------------------------------------------------------------------

install(
    TARGETS
        pcraster_dal
    RUNTIME
        DESTINATION lib COMPONENT Runtime
    LIBRARY
        DESTINATION lib COMPONENT Runtime
)
