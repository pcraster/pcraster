add_executable(oldcalc
    main
)

target_link_libraries(oldcalc
    PRIVATE
        pcraster::old_calc
)

install(
    TARGETS oldcalc
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
)
