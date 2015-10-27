set(SOURCES
    dev_CommandLineApplication.cc
    dev_CurlClient.cc
    dev_FilesystemUtils.cc
    dev_PythonClient.cc
    dev_Utils.cc
    dev_XercesClient.cc
)
if(GDAL_FOUND)
    SET(SOURCES
        ${SOURCES}
        dev_GDalClient.cc
    )
endif(GDAL_FOUND)
add_library(utils
    ${SOURCES}
)
target_link_libraries(utils
    ${Boost_FILESYSTEM_LIBRARY}
    ${Boost_PROGRAM_OPTIONS_LIBRARY}
    ${Boost_SYSTEM_LIBRARY}
)


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        dev_CommandLineApplicationTest
        dev_QtClientTest
        dev_UtilsTest
    )
    add_unit_tests(
        SCOPE utils
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            utils
            Qt4::QtCore
            ${Boost_PROGRAM_OPTIONS_LIBRARY}
    )
endif()
