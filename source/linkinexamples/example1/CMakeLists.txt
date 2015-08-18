include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

SET(SOURCES
   example1.c
)
ADD_LIBRARY(example1
    SHARED
    ${SOURCES}
)

ADD_CUSTOM_COMMAND(
    TARGET
        example1
    POST_BUILD
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/example1.xml
                ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}/
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/example1.xml
                ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}/noCodeLinkIn.xml
)
