set(_SOURCES
    mapmaximum
    mapminimum
)

add_library(multicore_field_python_totals
    STATIC
        ${_SOURCES}
)

target_include_directories(multicore_field_python_totals
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcraster_model_engine
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcraster_python
)
