configure_file(
    test_prolog.py.in
    test_prolog.py
    @ONLY
)

# Dependencies for the unit tests...
add_test(
    NAME
        setup_arrayed_variables_dependencies
    COMMAND
        "${CMAKE_COMMAND}"
        --build "${CMAKE_BINARY_DIR}"
        --config "$<CONFIG>"
        --target
                mapattr col2map _pcraster python_arrayed_variables_dummy
                generate_examples.sentinel python_modelling_framework_dummy
)

set_tests_properties(setup_arrayed_variables_dependencies
    PROPERTIES
        FIXTURES_SETUP
            FixtureArrayedVariablesDependencies
)

add_test(
    NAME
        setup_arrayed_variables
    COMMAND
        ${Python_EXECUTABLE} test_prolog.py
)

set_tests_properties(setup_arrayed_variables
    PROPERTIES
        FIXTURES_SETUP
            FixturesArrayedVariables
)

add_test(
    NAME
        python_arrayed_variables_unit_tests
    COMMAND
        ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/unitTests.py
)

# Environment variables to execute this unit test via ctest properly
set(test_env
    PATH=path_list_prepend:$<$<PLATFORM_ID:Windows>:$<TARGET_FILE_DIR:pcrcalc>>
    PYTHONPATH=path_list_prepend:${CMAKE_CURRENT_BINARY_DIR}
    PYTHONPATH=path_list_prepend:$<TARGET_FILE_DIR:pcrcalc>
)

set_tests_properties(python_arrayed_variables_unit_tests
    PROPERTIES
        FIXTURES_REQUIRED
            "FixtureArrayedVariablesDependencies;FixturesArrayedVariables"
        ENVIRONMENT_MODIFICATION
            "${test_env}"
  )
