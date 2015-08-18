include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

SET(SOURCES
    example2.c
)
ADD_LIBRARY(example2
    SHARED
    ${SOURCES}
)

ADD_CUSTOM_COMMAND(
    TARGET
        example2
    POST_BUILD
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/example2.xml
                ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}
)
