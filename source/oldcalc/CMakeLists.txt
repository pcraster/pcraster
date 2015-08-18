include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_old_calc
)
SET(SOURCES
    main.cc
)
ADD_EXECUTABLE(oldcalc
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(oldcalc
    pcraster_old_calc
)

INSTALL(
    TARGETS oldcalc
    RUNTIME DESTINATION bin COMPONENT Runtime
)
