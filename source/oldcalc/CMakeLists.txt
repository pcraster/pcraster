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
        DESTINATION bin COMPONENT Runtime
)
