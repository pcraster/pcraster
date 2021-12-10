add_library(exampleCpp SHARED
    example.cc
    xml.cc
)

target_include_directories(exampleCpp
    PRIVATE
        $<TARGET_PROPERTY:pcraster::raster_format,INTERFACE_INCLUDE_DIRECTORIES>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../include>
)

target_link_libraries(exampleCpp
    PRIVATE
        pcraster::xsd
)

set_target_properties(exampleCpp
    PROPERTIES
        DEBUG_POSTFIX ""
)

add_custom_command(
    TARGET
        exampleCpp
    POST_BUILD
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy ${CMAKE_CURRENT_SOURCE_DIR}/exampleCpp.xml
                $<TARGET_FILE_DIR:exampleCpp>
)
