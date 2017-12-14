add_library(multicore_field_python_type_conversions STATIC
    boolean
    nominal
    ordinal
    scalar
    nonspatial_field
)

add_library(pcraster::multicore_python_type_conversion
    ALIAS multicore_field_python_type_conversions
)

target_include_directories(multicore_field_python_type_conversions
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../..>
        $<TARGET_PROPERTY:pcraster::model_engine,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::python,INTERFACE_INCLUDE_DIRECTORIES>
    PUBLIC
        ${FERN_INCLUDE_DIRS}
)
