configure_file(scripts/__init__.py.in
    ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/__init__.py
    COPYONLY
)

configure_file(scripts/operations.py.in
    ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/_operations.py
    COPYONLY
)

configure_file(scripts/operators.py.in
    ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/_operators.py
    COPYONLY
)


pybind11_add_module(_pcraster_multicore MODULE THIN_LTO
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

target_include_directories(_pcraster_multicore
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../..>
)

# configure_python_extension(_pcraster_multicore
#     "_pcraster_multicore"
# )

target_link_libraries(_pcraster_multicore
    PRIVATE
        pcraster::python::utils
        fern_algorithm
        Python::Python
)

set_target_properties(_pcraster_multicore
    PROPERTIES
        PREFIX "${PYTHON_MODULE_PREFIX}"
        SUFFIX "${PYTHON_MODULE_EXTENSION}"
)

add_dependencies(_pcraster_multicore
    generate_examples.sentinel
    _pcraster
    fill_python_directory_structure # Refactor
)

set_target_properties(_pcraster_multicore
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            ${PROJECT_BINARY_DIR}/bin/pcraster/multicore
        LIBRARY_OUTPUT_DIRECTORY
            ${PROJECT_BINARY_DIR}/bin/pcraster/multicore
        INSTALL_RPATH
            "\\\$ORIGIN/../../../lib"
)

install(
    TARGETS _pcraster_multicore
    LIBRARY DESTINATION python/pcraster/multicore
    RUNTIME DESTINATION python/pcraster/multicore
)

install(
    FILES
        ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/__init__.py
        ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/_operators.py
        ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/_operations.py
    DESTINATION
        python/pcraster/multicore
)
