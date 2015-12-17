include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/..
    ${CMAKE_CURRENT_SOURCE_DIR}/../../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrgeo
)
set(TEST_NAMES
    mldd_dagrastertest
    mldd_outedgeiteratortest
    mldd_vertexiteratortest.cc
)
add_unit_tests(
    SCOPE mldd
    NAMES ${TEST_NAMES}
    LINK_LIBRARIES
        mldd
    DEPENDENCIES
        example1
        example2
        exampleCpp
)
