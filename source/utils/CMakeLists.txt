add_library(utils STATIC
    dev_CommandLineApplication.cc
    dev_FilesystemUtils.cc
    dev_GDalClient.cc
    # dev_PythonClient.cc
    dev_Utils.cc
    dev_XercesClient.cc
)

add_library(pcraster::utils ALIAS utils)

target_include_directories(utils
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../include>
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
)

target_include_directories(utils SYSTEM
    PUBLIC
        ${Boost_INCLUDE_DIRS}
)

target_link_libraries(utils
    PRIVATE
        XercesC::XercesC
        GDAL::GDAL
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE utils
        NAMES
            dev_CommandLineApplicationTest.cc
            dev_QtClientTest.cc
            dev_UtilsTest.cc
        LINK_LIBRARIES
            pcraster::utils
            Qt::Core
    )
endif()
