if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        dev_MathUtilsTest
    )
    add_unit_tests(
        SCOPE math_utils
        NAMES ${TEST_NAMES}
    )
endif()
