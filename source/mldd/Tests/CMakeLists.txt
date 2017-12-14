add_unit_tests(
    SCOPE mldd
    NAMES ${TEST_NAMES}
        mldd_dagrastertest
        mldd_outedgeiteratortest
        mldd_vertexiteratortest
    LINK_LIBRARIES
        pcraster::mldd
    DEPENDENCIES
        example1
        example2
        exampleCpp
)
