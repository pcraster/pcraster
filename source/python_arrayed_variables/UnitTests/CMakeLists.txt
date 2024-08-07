# TODO Debug build on Windows still needs to be checked
# if(NOT (WIN32 AND (${CMAKE_BUILD_TYPE} STREQUAL "Debug")))
#
#   configure_file(
#     ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
#     ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
#     @ONLY
#   )

    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    add_test(
        NAME setup_arrayed_variables
        COMMAND ${Python_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_arrayed_variables
        PROPERTIES FIXTURES_SETUP FixturesArrayedVariables
    )


  add_test(
    NAME
         python_arrayed_variables_unit_tests
    COMMAND
        ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/unitTests.py
  )

  set_tests_properties(python_arrayed_variables_unit_tests PROPERTIES
    DEPENDS
      generate_examples.sentinel
    DEPENDS
      mapattr
    DEPENDS
      col2map
    DEPENDS
      _pcraster
    DEPENDS
      python_modelling_framework_dummy
    DEPENDS
      python_arrayed_variables_dummy
    FIXTURES_REQUIRED
      FixturesArrayedVariables
  )


  # add path of project's unit test related files to PYTHONPATH
  # required to execute this unit test via ctest properly
  if(WIN32)
    set(ENV_PATH $ENV{PATH})
    string(REPLACE ";" "\\;" ENV_PATH "${ENV_PATH}")
    # argument is a 'cmake' ; separated list of values, therefore escape the environment path separator
    set_tests_properties(python_arrayed_variables_unit_tests PROPERTIES
      ENVIRONMENT "PATH=${ENV_PATH}\\;$<TARGET_FILE_DIR:pcrcalc>;PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;$<TARGET_FILE_DIR:pcrcalc>"
    )
  else()
    set_tests_properties(python_arrayed_variables_unit_tests PROPERTIES
      ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:$<TARGET_FILE_DIR:pcrcalc>"
    )
  endif()

# endif()
