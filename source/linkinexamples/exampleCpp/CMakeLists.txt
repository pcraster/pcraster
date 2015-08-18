include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
    ${CMAKE_CURRENT_BINARY_DIR}/../../pcraster_xsd
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_xsd
)

SET(SOURCES
    example.cc
    xml.cc
)

ADD_LIBRARY(exampleCpp
    SHARED
    ${SOURCES}
)

TARGET_LINK_LIBRARIES(exampleCpp
    pcraster_xsd
    ${XercesC_LIBRARIES}
)

ADD_CUSTOM_COMMAND(
    TARGET
        exampleCpp
    POST_BUILD
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/exampleCpp.xml
                ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}/
)
