include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
)

SET(SOURCES
    dirconv.c
    dirstat.c
    ran.c
    recipes.c
    rint.c
)
SET(TEST_SOURCES
    dirconvtest.cc
    mathx_test.cc
)
ADD_LIBRARY(mathx
    ${SOURCES}
)
ADD_EXECUTABLE(test_mathx
    ${TEST_SOURCES}
)
TARGET_LINK_LIBRARIES(test_mathx
    mathx
)
ADD_CUSTOM_TARGET(run_test_mathx
    COMMAND test_mathx
    DEPENDS test_mathx
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
ADD_DEPENDENCIES(tests run_test_mathx)
