add_library(utils STATIC
    dev_CommandLineApplication
    dev_FilesystemUtils
    dev_GDalClient
    dev_PythonClient
    dev_Utils
    dev_XercesClient
)

add_library(pcraster::utils ALIAS utils)

target_include_directories(utils SYSTEM
    PRIVATE
        ${GDAL_INCLUDE_DIRS}
        ${PYTHON_INCLUDE_DIR}
)

target_include_directories(utils
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
)

target_include_directories(utils
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(utils
    PRIVATE
        Boost::filesystem
        Boost::program_options
        XercesC::XercesC
        ${GDAL_LIBRARIES}
        ${PYTHON_LIBRARIES}
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE utils
        NAMES
            dev_CommandLineApplicationTest
            dev_QtClientTest
            dev_UtilsTest
        LINK_LIBRARIES
            pcraster::utils
            Qt5::Core
    )
endif()
