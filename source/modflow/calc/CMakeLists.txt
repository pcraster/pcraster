add_library(pcraster_modflow SHARED
    xml
    pcr_mflink
)

add_library(pcraster::modflow ALIAS pcraster_modflow)

set_target_properties(pcraster_modflow
    PROPERTIES
        SOVERSION 0.1
)

target_link_libraries(pcraster_modflow
    PRIVATE
        pcraster::pcrmf
)

add_custom_command(
    TARGET
        pcraster_modflow
    POST_BUILD
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/pcraster_modflow.xml
                ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}/
)

install(
    TARGETS pcraster_modflow
    RUNTIME
        DESTINATION lib COMPONENT Runtime
    LIBRARY
        DESTINATION lib COMPONENT Runtime
)

IF(WIN32)
    INSTALL(
        FILES
            pcraster_modflow.xml
        DESTINATION
            bin
    )
ELSE(WIN32)
    INSTALL(
        FILES
            pcraster_modflow.xml
        DESTINATION
            lib
    )
ENDIF(WIN32)


MATH(EXPR NR_BITS "${CMAKE_SIZEOF_VOID_P} * 8")

IF(WIN32)
  SET(EXE_EXTENSION ".exe")
ELSE(WIN32)
  SET(EXE_EXTENSION "")
ENDIF(WIN32)

SET(MF_BINARY ${PROJECT_SOURCE_DIR}/external/${CMAKE_SYSTEM_NAME}/${NR_BITS}/mf2005${EXE_EXTENSION})

INSTALL(
  PROGRAMS
    ${MF_BINARY}
  DESTINATION
    bin
)


if(PCRASTER_BUILD_TEST)

  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/test_prolog.py.in
    ${CMAKE_CURRENT_BINARY_DIR}/test_prolog.py
    @ONLY
  )
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/../tests/tests.py.in
    ${CMAKE_CURRENT_BINARY_DIR}/tests.py
    @ONLY
  )

  set(TEST_NAMES
    calc_DemoTest
  )

  add_unit_tests(
    SCOPE modflow
    NAMES ${TEST_NAMES}
    INCLUDE_DIRS
        ${PYTHON_INCLUDE_DIR}
    LINK_LIBRARIES
        pcraster::model_engine
  )

  # the pcraster_modflow dll must be build first
  ADD_DEPENDENCIES(modflow_calc_DemoTest
      pcraster_modflow
      fill_python_directory_structure
  )

  # modflow binary must be copied to execute unit tests
  ADD_CUSTOM_COMMAND(
    TARGET
      modflow_calc_DemoTest
    COMMAND ${CMAKE_COMMAND}
      ARGS -E copy ${MF_BINARY} ${EXECUTABLE_OUTPUT_PATH}
  )

    add_test(
        NAME setup_modflow
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_modflow
        PROPERTIES FIXTURES_SETUP FixtureModflow
    )

  add_test(
    NAME
         modflow_python_unit_tests
    COMMAND
        ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/tests.py
  )

  set_tests_properties(modflow_python_unit_tests PROPERTIES
    DEPENDS
      # calc demo test builds some required inputs
      modflow_calc_DemoTest
  )

    set_tests_properties(modflow_calc_DemoTest PROPERTIES
        FIXTURES_REQUIRED
          FixtureModflow
    )


  # add path of project's unit test related files to PYTHONPATH
  # required to execute this unit test via ctest properly
  if(WIN32)
    set(ENV_PATH $ENV{PATH})
    string(REPLACE ";" "\\;" ENV_PATH "${ENV_PATH}")
    # argument is a 'cmake' ; separated list of values, therefore escape the environment path separator
    set_tests_properties(modflow_calc_DemoTest modflow_python_unit_tests PROPERTIES
      ENVIRONMENT "PATH=${ENV_PATH}\\;${EXECUTABLE_OUTPUT_PATH}\\;${CMAKE_CURRENT_BINARY_DIR};PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;${EXECUTABLE_OUTPUT_PATH}"
    )
  else()
    set_tests_properties(modflow_calc_DemoTest modflow_python_unit_tests PROPERTIES
      ENVIRONMENT "PATH=${EXECUTABLE_OUTPUT_PATH}:${CMAKE_CURRENT_BINARY_DIR}:$ENV{PATH};PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:${EXECUTABLE_OUTPUT_PATH}"
    )
  endif()

endif()
