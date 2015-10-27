CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../src
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_model_engine
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrdiscr
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrcom
    ${CMAKE_CURRENT_BINARY_DIR}/../../pcraster_xsd
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_xsd
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_dal
)

SET(SOURCES
    xml.cc
    pcr_mflink.cc
)

ADD_LIBRARY(pcraster_modflow
    SHARED
    ${SOURCES}
)

SET_TARGET_PROPERTIES(pcraster_modflow
    PROPERTIES
    SOVERSION 0.1
)

TARGET_LINK_LIBRARIES(pcraster_modflow
    pcrmf
    pcrdiscr
    pcraster_model_engine
    # ${Boost_SIGNALS_LIBRARY}
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


if(FALSE AND PCRASTER_BUILD_TEST)
    SET(TEST_SOURCES
        calc_Test.cc
        calc_DemoTest.cc
    )
    ADD_EXECUTABLE(test_pcraster_modflow
        ${TEST_SOURCES}
    )

    # the pcraster_modflow dll must be build first
    ADD_DEPENDENCIES(test_pcraster_modflow pcraster_modflow)

    TARGET_LINK_LIBRARIES(test_pcraster_modflow
        pcraster_model_engine
        pcrgeo
        ${Boost_PYTHON_LIBRARY}
        ${PYTHON_LIBRARIES}
    )

    ADD_CUSTOM_TARGET(run_test_pcraster_modflow
        COMMAND bash ./testrun.prolog
        COMMAND test_pcraster_modflow
        DEPENDS test_pcraster_modflow
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )

    # modflow binary must be copied to bin directory for unit tests
    MATH(EXPR NR_BITS "${CMAKE_SIZEOF_VOID_P} * 8")

    IF(WIN32)
      SET(EXE_EXTENSION ".exe")
    ELSE(WIN32)
      SET(EXE_EXTENSION "")
    ENDIF(WIN32)

    SET(MF_BINARY ${PROJECT_SOURCE_DIR}/external/${CMAKE_SYSTEM_NAME}/${NR_BITS}/pcrmf2k${EXE_EXTENSION})

    ADD_CUSTOM_COMMAND(
        TARGET
            test_pcraster_modflow
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${MF_BINARY} ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}/
    )
    ADD_DEPENDENCIES(run_test_pcraster_modflow test_pcraster_modflow)
    ADD_DEPENDENCIES(run_test_pcraster_modflow fill_python_directory_structure)
    ADD_DEPENDENCIES(tests run_test_pcraster_modflow)
endif()
