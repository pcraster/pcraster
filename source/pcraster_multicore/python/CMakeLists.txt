add_subdirectory(local)
add_subdirectory(focal)
add_subdirectory(total)
add_subdirectory(type_conversion)
add_subdirectory(scripts)

add_library(_pcraster_multicore SHARED
    execution_policy
    pcraster_multicore
)

if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    # otherwise compilation with hidden visibility flags will fail
    if(${Boost_MINOR_VERSION} EQUAL 65)
        target_compile_definitions(
            _pcraster_multicore
        PUBLIC
            -DBOOST_PYTHON_USE_GCC_SYMBOL_VISIBILITY)
    endif()
endif()

target_include_directories(_pcraster_multicore
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../..>
        ${PYTHON_INCLUDE_DIR}
)

configure_python_extension(_pcraster_multicore
    "_pcraster_multicore"
)

target_link_libraries(_pcraster_multicore
    pcraster::multicore_python_local
    multicore_field_python_totals
    multicore_field_python_focals
    multicore_field_python_locals
    multicore_field_python_type_conversions
    pcraster::python::utils
    ${PCR_BOOST_PYTHON_NUMPY}
    ${PCR_BOOST_PYTHON}
    Threads::Threads
    ${FERN_LIBRARIES}
)

set_target_properties(_pcraster_multicore
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            ${PROJECT_BINARY_DIR}/bin/pcraster/multicore
        LIBRARY_OUTPUT_DIRECTORY
            ${PROJECT_BINARY_DIR}/bin/pcraster/multicore
)
