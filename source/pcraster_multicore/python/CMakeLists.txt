add_subdirectory(scripts)

add_library(_pcraster_multicore SHARED
    execution_policy
    pcraster_multicore

    focal/slope
    focal/window4total
    focal/windowtotal

    local/and
    local/or
    local/xor
    local/not
    local/defined

    local/add
    local/sub
    local/mul
    local/div

    local/sqrt
    local/pow
    local/sqr
    local/abs
    local/fac
    local/ln
    local/log10
    local/roundup
    local/rounddown
    local/roundoff

    local/equal
    local/unequal
    local/less
    local/less_equal
    local/greater
    local/greater_equal

    local/ifthen
    local/ifthenelse

    local/cover
    local/minimum
    local/maximum

    local/cos
    local/sin
    local/tan
    local/acos
    local/asin
    local/atan

    local/utils

    total/mapmaximum
    total/mapminimum

    type_conversion/boolean
    type_conversion/nominal
    type_conversion/ordinal
    type_conversion/scalar
    type_conversion/nonspatial_field
)

fix_boost_python_visibility(_pcraster_multicore)

target_include_directories(_pcraster_multicore
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../..>
        ${PYTHON_INCLUDE_DIR}
)

configure_python_extension(_pcraster_multicore
    "_pcraster_multicore"
)

target_link_libraries(_pcraster_multicore
    PRIVATE
        pcraster::python::utils
        ${PCR_BOOST_PYTHON_NUMPY}
        ${PCR_BOOST_PYTHON}
        fern_algorithm
)

add_dependencies(_pcraster_multicore
    generate_examples.sentinel
    _pcraster
    fill_python_directory_structure # Refactor
    _pcraster_modflow_python        # Refactor
)

set_target_properties(_pcraster_multicore
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            ${PROJECT_BINARY_DIR}/bin/pcraster/multicore
        LIBRARY_OUTPUT_DIRECTORY
            ${PROJECT_BINARY_DIR}/bin/pcraster/multicore
)
