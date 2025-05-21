configure_file(scripts/__init__.py.in
    __init__.py
    COPYONLY
)

configure_file(scripts/operations.py.in
    _operations.py
    COPYONLY
)

configure_file(scripts/operators.py.in
    _operators.py
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

target_include_directories(_pcraster_multicore
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../..>
)

target_link_libraries(_pcraster_multicore
    PRIVATE
        pcraster::python::utils
        fern_algorithm
)


set_target_properties(_pcraster_multicore
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/multicore
        LIBRARY_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/multicore
        OUTPUT_NAME
            "_pcraster_moc"
        INSTALL_RPATH
            "$ORIGIN/../../../lib"
        DEBUG_POSTFIX
            ""
        PREFIX
            "${PYTHON_MODULE_PREFIX}"
        SUFFIX
            "${PYTHON_MODULE_EXTENSION}"
)

add_dependencies(_pcraster_multicore
    _pcraster
)

add_custom_command(
    TARGET _pcraster_multicore POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${CMAKE_CURRENT_BINARY_DIR}/__init__.py
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/multicore/__init__.py
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${CMAKE_CURRENT_BINARY_DIR}/_operators.py
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/multicore/_operators.py
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${CMAKE_CURRENT_BINARY_DIR}/_operations.py
            $<TARGET_FILE_DIR:pcrcalc>/pcraster/multicore/_operations.py
)


if(PCRASTER_BUILD_TEST)
  add_dependencies(_pcraster_multicore
      generate_examples.sentinel
  )
endif()


install(
    TARGETS _pcraster_multicore
    LIBRARY DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/multicore
    RUNTIME DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/multicore
)

install(
    FILES
        $<TARGET_FILE_DIR:pcrcalc>/pcraster/multicore/__init__.py
        $<TARGET_FILE_DIR:pcrcalc>/pcraster/multicore/_operators.py
        $<TARGET_FILE_DIR:pcrcalc>/pcraster/multicore/_operations.py
    DESTINATION
        ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster/multicore
)
