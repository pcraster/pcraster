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

add_library(pcraster::multicore_python_local
    ALIAS multicore_field_python_locals)

target_include_directories(multicore_field_python_locals
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../..>
        $<TARGET_PROPERTY:pcraster::model_engine,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::python,INTERFACE_INCLUDE_DIRECTORIES>
)
