SET(SOURCES
    cross.c
    looktab.c
    lookup.c
    readltab.c
    table.c
    writltab.c
)
ADD_LIBRARY(tab
    ${SOURCES}
)
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
)

SET(TEST_SOURCES
    tab_test.cc
    tab_classcountmaptest.cc
    tab_classcountmaptest.h
    tab_classintervalmaptest.cc
    tab_classintervalmaptest.h
)

ADD_EXECUTABLE(test_tab
    ${TEST_SOURCES}
)

TARGET_LINK_LIBRARIES(test_tab
    pcrcom
)

ADD_CUSTOM_TARGET(run_test_tab
    COMMAND test_tab
    DEPENDS test_tab
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
ADD_DEPENDENCIES(tests run_test_tab)
