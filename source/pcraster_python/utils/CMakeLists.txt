add_library(pcraster_python_utils STATIC
    Globals.cc
)

add_library(pcraster::python::utils ALIAS pcraster_python_utils)

generate_export_header(pcraster_python_utils
    BASE_NAME pcraster_python_utils
)

target_include_directories(pcraster_python_utils
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcraster_python_utils
    PUBLIC
        pcraster::model_engine
)
