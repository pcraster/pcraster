configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/test_prolog.py.in
    ${CMAKE_CURRENT_BINARY_DIR}/test_prolog.py
    @ONLY
)

# Dependencies for the unit tests...
add_test(setup_python_modelling_framework_dependencies
    "${CMAKE_COMMAND}"
    --build "${CMAKE_BINARY_DIR}"
    --config "$<CONFIG>"
    --target
            mapattr col2map _pcraster
            generate_examples.sentinel python_modelling_framework_dummy
)

set_tests_properties(setup_python_modelling_framework_dependencies
    PROPERTIES
        FIXTURES_SETUP
            FixturePythonModellingFrameworkDependencies
)

add_test(
    NAME
        setup_python_modelling_framework
    COMMAND
        ${Python_EXECUTABLE} test_prolog.py
)

set_tests_properties(setup_python_modelling_framework
    PROPERTIES
        FIXTURES_SETUP
            FixturePythonModellingFramework
)

add_test(
    NAME
        python_modelling_framework_unit_tests
    COMMAND
        ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/unitTests.py
)

# Environment variables to execute this unit test via ctest properly
set(test_env
    PATH=path_list_prepend:$<$<PLATFORM_ID:Windows>:$<TARGET_FILE_DIR:pcrcalc>>
    PYTHONPATH=path_list_prepend:${CMAKE_CURRENT_BINARY_DIR}
    PYTHONPATH=path_list_prepend:$<TARGET_FILE_DIR:pcrcalc>
)

set_tests_properties(python_modelling_framework_unit_tests
    PROPERTIES
        FIXTURES_REQUIRED
            "FixturePythonModellingFrameworkDependencies;FixturePythonModellingFramework"
        ENVIRONMENT_MODIFICATION
            "${test_env}"
)
