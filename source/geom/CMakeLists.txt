include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
)

SET(SOURCES
    point.c
    rect.c
)

set_source_files_properties(${SOURCES} PROPERTIES LANGUAGE CXX)

ADD_LIBRARY(geom
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(geom
    misc
)

SET(TEST_SOURCES
    geom_test.cc
    geom_pointtest.cc
)
ADD_EXECUTABLE(test_geom
    ${TEST_SOURCES}
)

TARGET_LINK_LIBRARIES(test_geom
    geom
)

ADD_CUSTOM_TARGET(run_test_geom
    COMMAND test_geom
    DEPENDS test_geom
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
ADD_DEPENDENCIES(tests run_test_geom)
