add_executable(pcrcalc
    main
)

target_link_libraries(pcrcalc
    PRIVATE
        pcraster::model_engine
)

install(
    TARGETS pcrcalc
    RUNTIME
        DESTINATION bin COMPONENT Runtime
)
