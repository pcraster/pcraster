add_library(multicore_field_python_focals STATIC
    slope
    window4total
    windowtotal
)

add_library(pcraster::multicore_python_focal
    ALIAS multicore_field_python_focals
)

target_include_directories(multicore_field_python_focals
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../..>
        $<TARGET_PROPERTY:pcraster::model_engine,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::python,INTERFACE_INCLUDE_DIRECTORIES>
)
