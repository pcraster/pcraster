set(_SOURCES

    and
    or
    xor
    not
    defined

    add
    sub
    mul
    div

    sqrt
    pow
    sqr
    abs
    fac
    ln
    log10
    roundup
    rounddown
    roundoff

    equal
    unequal
    less
    less_equal
    greater
    greater_equal

    ifthen
    ifthenelse

    cover
    minimum
    maximum

    cos
    sin
    tan
    acos
    asin
    atan

    utils
)

add_library(multicore_field_python_locals
    STATIC
        ${_SOURCES}
)

target_include_directories(multicore_field_python_locals
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcrcom
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcraster_model_engine
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../pcraster_python
)
