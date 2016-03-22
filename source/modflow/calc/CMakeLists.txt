CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
)

SET(SOURCES
    xml.cc
    pcr_mflink.cc
)

ADD_LIBRARY(pcraster_modflow
    SHARED
    ${SOURCES}
)

target_include_directories(pcraster_modflow
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../src
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_model_engine
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrdiscr
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_xsd
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_dal
    ${CMAKE_CURRENT_BINARY_DIR}/../../pcraster_xsd
)

SET_TARGET_PROPERTIES(pcraster_modflow
    PROPERTIES
    SOVERSION 0.1
)

TARGET_LINK_LIBRARIES(pcraster_modflow
    pcrmf
    pcrdiscr
    pcraster_model_engine
)

ADD_CUSTOM_COMMAND(
    TARGET
        pcraster_modflow
    POST_BUILD
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/pcraster_modflow.xml
                ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}/
)


# Install rules.
INSTALL(
    TARGETS pcraster_modflow
    RUNTIME DESTINATION lib COMPONENT Runtime
    LIBRARY DESTINATION lib COMPONENT Runtime
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

SET(MF_BINARY ${PROJECT_SOURCE_DIR}/external/${CMAKE_SYSTEM_NAME}/${NR_BITS}/pcrmf2k${EXE_EXTENSION})

INSTALL(
  FILES
    ${MF_BINARY}
  DESTINATION
    bin
)


if(PCRASTER_BUILD_TEST)

  set(TEST_NAMES
    calc_DemoTest
  )

  add_unit_tests(
    SCOPE modflow
    NAMES ${TEST_NAMES}
    LINK_LIBRARIES
      pcraster_model_engine
      pcrgeo
      ${Boost_PYTHON_LIBRARY}
      ${PYTHON_LIBRARIES}
  )

  target_include_directories(modflow_calc_DemoTest
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_model_engine
      ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrgeo
      ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrcom
  )

  # the pcraster_modflow dll must be build first
  ADD_DEPENDENCIES(modflow_calc_DemoTest pcraster_modflow)
  ADD_DEPENDENCIES(modflow_calc_DemoTest fill_python_directory_structure)

  # modflow binary must be copied to execute unit tests
  ADD_CUSTOM_COMMAND(
    TARGET
      modflow_calc_DemoTest
    COMMAND ${CMAKE_COMMAND}
      ARGS -E copy ${MF_BINARY} ${CMAKE_CURRENT_BINARY_DIR}/
  )


  # add path of project's unit test related files to PYTHONPATH
  # required to execute this unit test via ctest properly
  if(WIN32)
    # argument is a 'cmake' ; separated list of values, therefore escape the environment path separator
    set_tests_properties(modflow_calc_DemoTest PROPERTIES
      ENVIRONMENT "PATH=${CMAKE_CURRENT_BINARY_DIR}/\\;$ENV{PATH};PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;${EXECUTABLE_OUTPUT_PATH}"
    )
  else()
    set_tests_properties(modflow_calc_DemoTest PROPERTIES
      ENVIRONMENT "PATH=${CMAKE_CURRENT_BINARY_DIR}/:$ENV{PATH};PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:${EXECUTABLE_OUTPUT_PATH}"
    )
  endif()

endif()
