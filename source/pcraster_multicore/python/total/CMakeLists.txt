add_library(multicore_field_python_totals STATIC
    mapmaximum
    mapminimum
)

add_library(pcraster::multicore_python_total
    ALIAS multicore_field_python_totals
)

target_include_directories(multicore_field_python_totals
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../..>
        $<TARGET_PROPERTY:pcraster::model_engine,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::python,INTERFACE_INCLUDE_DIRECTORIES>
    PUBLIC
        ${FERN_INCLUDE_DIRS}
)
