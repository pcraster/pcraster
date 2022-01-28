add_unit_tests(
    SCOPE mldd
    NAMES ${TEST_NAMES}
        mldd_dagrastertest.cc
        mldd_outedgeiteratortest.cc
        mldd_vertexiteratortest.cc
    LINK_LIBRARIES
        pcraster::mldd
    DEPENDENCIES
        example1
        example2
        exampleCpp
)
