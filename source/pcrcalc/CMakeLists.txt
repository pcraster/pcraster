add_executable(pcrcalc
    main.cc
)

target_link_libraries(pcrcalc
    PRIVATE
        pcraster::model_engine
)

install(
    TARGETS pcrcalc
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
)
