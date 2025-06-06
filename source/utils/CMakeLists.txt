file(CONFIGURE
    OUTPUT
        current_dev_year.inc
    CONTENT
        "\"Copyright (C) 2008-@DEV_CURRENT_YEAR@ \""
    ESCAPE_QUOTES
    @ONLY
)

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
        ${CMAKE_CURRENT_BINARY_DIR}/
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../include>
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
)

target_link_libraries(utils
    PRIVATE
        XercesC::XercesC
        GDAL::GDAL
        Boost::headers
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
