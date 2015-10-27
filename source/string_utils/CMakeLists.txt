if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        dev_ToStringTest
    )
    add_unit_tests(
        SCOPE string_utils
        NAMES ${TEST_NAMES}
    )
endif()
