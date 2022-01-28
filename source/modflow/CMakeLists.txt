add_subdirectory(src)
add_subdirectory(calc)
add_subdirectory(python)


set(MF_BINARY ${PROJECT_SOURCE_DIR}/external/${CMAKE_SYSTEM_NAME}/64/mf2005${CMAKE_EXECUTABLE_SUFFIX})

IF(WIN32)
  INSTALL(
    PROGRAMS
      ${MF_BINARY}
    DESTINATION
      ${PCRASTER_BIN_INSTALL_DIR}
  )
ELSE()

  IF(PCRASTER_BUILD_MODFLOW_EXECUTABLE)
    add_subdirectory(mf2005)
  ELSE()
    INSTALL(
      PROGRAMS
        ${MF_BINARY}
      DESTINATION
        ${PCRASTER_BIN_INSTALL_DIR}
    )
  ENDIF()

ENDIF()


if(PCRASTER_BUILD_TEST AND NOT PCRASTER_BUILD_MODFLOW_EXECUTABLE)

  add_custom_target(modflow_exe_dummy_target ALL)

  # modflow binary must be copied to execute unit tests
  ADD_CUSTOM_COMMAND(
    TARGET
      modflow_exe_dummy_target
    COMMAND ${CMAKE_COMMAND} ARGS -E copy ${MF_BINARY} $<TARGET_FILE_DIR:pcraster_modflow>
  )

endif()
