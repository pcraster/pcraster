add_library(example1 SHARED
   example1.c
)

target_include_directories(example1
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../include>
)

set_target_properties(example1
    PROPERTIES
        DEBUG_POSTFIX ""
)

add_custom_command(
    TARGET
        example1
    POST_BUILD
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/example1.xml
                $<TARGET_FILE_DIR:example1>
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/example1.xml
                $<TARGET_FILE_DIR:example1>/noCodeLinkIn.xml
)
