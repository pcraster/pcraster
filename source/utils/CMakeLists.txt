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

set(TEST_SOURCES
    dev_Test.cc
    dev_CommandLineApplicationTest.cc
    dev_QtClientTest.cc
    dev_UtilsTest.cc
)
add_executable(test_utils
    ${TEST_SOURCES}
)
target_link_libraries(test_utils
    utils
    ${Boost_FILESYSTEM_LIBRARY}
    ${Boost_PROGRAM_OPTIONS_LIBRARY}
    ${Boost_SYSTEM_LIBRARY}
    Qt4::QtCore
)
add_test(utils ${EXECUTABLE_OUTPUT_PATH}/test_utils)
add_custom_target(run_test_utils
    test_utils
    DEPENDS test_utils
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
add_dependencies(tests run_test_utils)
