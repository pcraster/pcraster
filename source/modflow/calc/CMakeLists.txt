add_library(pcraster_modflow SHARED
    xml.cc
    pcr_mflink.cc
)

add_library(pcraster::modflow ALIAS pcraster_modflow)

set_target_properties(pcraster_modflow
    PROPERTIES
        SOVERSION ${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}
)

target_link_libraries(pcraster_modflow
    PRIVATE
        pcraster::pcrmf_calc
)

add_custom_command(
    TARGET
        pcraster_modflow
    POST_BUILD
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/pcraster_modflow.xml
                $<TARGET_FILE_DIR:pcraster_modflow>
)

install(
    TARGETS pcraster_modflow
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
    LIBRARY
        DESTINATION ${PCRASTER_LIB_INSTALL_DIR} COMPONENT Runtime
)

IF(WIN32)
    INSTALL(
        FILES
            pcraster_modflow.xml
        DESTINATION
            ${PCRASTER_BIN_INSTALL_DIR}
    )
ELSE(WIN32)
    INSTALL(
        FILES
            pcraster_modflow.xml
        DESTINATION
            ${PCRASTER_LIB_INSTALL_DIR}
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
    ${PCRASTER_BIN_INSTALL_DIR}
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
    calc_DemoTest.cc
  )

  add_unit_tests(
    SCOPE modflow
    NAMES ${TEST_NAMES}
    INCLUDE_DIRS
        ${Python3_INCLUDE_DIRS}
    LINK_LIBRARIES
        pcraster::model_engine
  )

  # the pcraster_modflow dll must be build first
  ADD_DEPENDENCIES(modflow_calc_DemoTest.cc
      pcraster_modflow
      _pcraster
  )

  # modflow binary must be copied to execute unit tests
  ADD_CUSTOM_COMMAND(
    TARGET
      modflow_calc_DemoTest.cc
    COMMAND ${CMAKE_COMMAND}
      ARGS -E copy ${MF_BINARY} $<TARGET_FILE_DIR:pcraster_modflow>
  )

    add_test(
        NAME setup_modflow
        COMMAND ${Python3_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_modflow
        PROPERTIES FIXTURES_SETUP FixtureModflow
    )

  add_test(
    NAME
         modflow_python_unit_tests
    COMMAND
        ${Python3_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/tests.py
  )

  set_tests_properties(modflow_python_unit_tests PROPERTIES
    DEPENDS
      # calc demo test builds some required inputs
      modflow_calc_DemoTest.cc
  )

    set_tests_properties(modflow_calc_DemoTest.cc PROPERTIES
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
      ENVIRONMENT "PATH=${ENV_PATH}\\;$<TARGET_FILE_DIR:pcrcalc>\\;${CMAKE_CURRENT_BINARY_DIR};PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;$<TARGET_FILE_DIR:pcrcalc>"
    )
  else()
    set_tests_properties(modflow_calc_DemoTest.cc modflow_python_unit_tests PROPERTIES
      ENVIRONMENT "PATH=$<TARGET_FILE_DIR:pcrcalc>:${CMAKE_CURRENT_BINARY_DIR}:$ENV{PATH};PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:$<TARGET_FILE_DIR:pcrcalc>"
    )
  endif()

endif()
