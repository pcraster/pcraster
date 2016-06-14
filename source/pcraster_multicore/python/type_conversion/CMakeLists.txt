set(_SOURCES
    boolean
    nominal
    ordinal
    scalar
    nonspatial_field
)

add_library(multicore_field_python_type_conversions
    STATIC
        ${_SOURCES}
)

target_include_directories(multicore_field_python_type_conversions
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcraster_model_engine
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcraster_python
)
