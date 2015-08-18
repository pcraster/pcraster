include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../utils
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_xsd
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_model_engine
)
SET(SOURCES
    main.cc
)
ADD_EXECUTABLE(pcrcalc
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(pcrcalc
    pcraster_model_engine
)

INSTALL(
    TARGETS pcrcalc
    RUNTIME DESTINATION bin COMPONENT Runtime
)
