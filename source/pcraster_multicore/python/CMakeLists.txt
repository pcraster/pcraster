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

# NO_EXTRAS required for success on macOS conda build
pybind11_add_module(_pcraster_multicore MODULE NO_EXTRAS THIN_LTO
    execution_policy.cc
    pcraster_multicore.cc

    focal/slope.cc
    focal/window4total.cc
    focal/windowtotal.cc

    local/and.cc
    local/or.cc
    local/xor.cc
    local/not.cc
    local/defined.cc

    local/add.cc
    local/sub.cc
    local/mul.cc
    local/div.cc

    local/sqrt.cc
    local/pow.cc
    local/sqr.cc
    local/abs.cc
    local/fac.cc
    local/ln.cc
    local/log10.cc
    local/roundup.cc
    local/rounddown.cc
    local/roundoff.cc

    local/equal.cc
    local/unequal.cc
    local/less.cc
    local/less_equal.cc
    local/greater.cc
    local/greater_equal.cc

    local/ifthen.cc
    local/ifthenelse.cc

    local/cover.cc
    local/minimum.cc
    local/maximum.cc

    local/cos.cc
    local/sin.cc
    local/tan.cc
    local/acos.cc
    local/asin.cc
    local/atan.cc

    local/utils.cc

    total/mapmaximum.cc
    total/mapminimum.cc

    type_conversion/boolean.cc
    type_conversion/nominal.cc
    type_conversion/ordinal.cc
    type_conversion/scalar.cc
    type_conversion/nonspatial_field.cc
)
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    # It seems that clang doesn't set flag this by default yet
    set_property(
        SOURCE
            pcraster_multicore.cc
            local/cover.cc
            local/minimum.cc
            local/maximum.cc
        PROPERTY
            COMPILE_OPTIONS
                -fsized-deallocation
    )
endif()


target_include_directories(_pcraster_multicore
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../..>
)

# configure_python_extension(_pcraster_multicore
#     "_pcraster_multicore"
# )

# TODO why is Python3::module not working on Linux?
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_link_libraries(_pcraster_multicore
        PRIVATE
            pcraster::python::utils
            fern_algorithm
            Python3::Python
    )
else()
        target_link_libraries(_pcraster_multicore
        PRIVATE
            pcraster::python::utils
            fern_algorithm
            Python3::Module
    )
endif()

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
    LIBRARY DESTINATION ${PCRASTER_PYTHON_INSTALL_PREFIX}/pcraster/multicore
    RUNTIME DESTINATION ${PCRASTER_PYTHON_INSTALL_PREFIX}/pcraster/multicore
)

install(
    FILES
        ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/__init__.py
        ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/_operators.py
        ${PROJECT_BINARY_DIR}/bin/pcraster/multicore/_operations.py
    DESTINATION
        ${PCRASTER_PYTHON_INSTALL_PREFIX}/pcraster/multicore
)
