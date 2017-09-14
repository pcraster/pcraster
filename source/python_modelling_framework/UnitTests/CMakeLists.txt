# TODO Debug build on Windows still needs to be checked
# if(NOT (WIN32 AND (${CMAKE_BUILD_TYPE} STREQUAL "Debug")))

#   configure_file(
#     ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
#     ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
#     @ONLY
#   )

  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/test_prolog.py.in
    ${CMAKE_CURRENT_BINARY_DIR}/test_prolog.py
    @ONLY
  )

    add_test(
        NAME setup_python_modelling_framework
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )

    add_test(
      NAME
         python_modelling_framework_unit_tests
      COMMAND
        ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/unitTests.py
    )

    set_tests_properties(setup_python_modelling_framework
        PROPERTIES FIXTURES_SETUP FixturePythonModellingFramework
    )

  set_tests_properties(python_modelling_framework_unit_tests PROPERTIES
    DEPENDS
      generate_examples.sentinel
    DEPENDS
      mapattr
    DEPENDS
      col2map
    DEPENDS
      fill_python_directory_structure
    DEPENDS
      python_modelling_framework_dummy
    FIXTURES_REQUIRED
      FixturePythonModellingFramework
  )




#   set(TEST_NAMES
#     unit_tests
#   )
#
#   add_unit_tests(
#     SCOPE python_modelling_framework
#     NAMES ${TEST_NAMES}
#     LINK_LIBRARIES
#       ${Boost_PYTHON_LIBRARY}
#       ${PYTHON_LIBRARIES}
#   )
#
#   add_dependencies(python_modelling_framework_unit_tests
#     examples.sentinel
#     mapattr col2map
#     fill_python_directory_structure
#     python_modelling_framework_dummy
#   )

  # add path of project's unit test related files to PYTHONPATH
  # required to execute this unit test via ctest properly
  if(WIN32)
    set(ENV_PATH $ENV{PATH})
    string(REPLACE ";" "\\;" ENV_PATH "${ENV_PATH}")
    # argument is a 'cmake' ; separated list of values, therefore escape the environment path separator
    set_tests_properties(python_modelling_framework_unit_tests PROPERTIES
      ENVIRONMENT "PATH=${ENV_PATH}\\;${EXECUTABLE_OUTPUT_PATH};PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;${EXECUTABLE_OUTPUT_PATH}"
    )
  else()
    set_tests_properties(python_modelling_framework_unit_tests PROPERTIES
      ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:${EXECUTABLE_OUTPUT_PATH}"
    )
  endif()

# endif()
