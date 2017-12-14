add_library(example2 SHARED
    example2
)

target_include_directories(example2
    PRIVATE
        $<TARGET_PROPERTY:pcraster::raster_format,INTERFACE_INCLUDE_DIRECTORIES>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../include>
)

add_custom_command(
    TARGET
        example2
    POST_BUILD
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/example2.xml
                ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}
)
