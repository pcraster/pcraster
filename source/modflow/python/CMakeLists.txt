if(CMAKE_COMPILER_IS_GNUCXX)
    SET_SOURCE_FILES_PROPERTIES(pcrmfpy.cc
        COMPILE_FLAGS
            "-Wno-strict-aliasing"
    )
endif()

add_library(_pcraster_modflow_python SHARED
    pcrmfpy.cc
)

add_library(pcraster::modflow_python ALIAS _pcraster_modflow_python)

if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    # otherwise compilation with hidden visibility flags will fail
    if(${Boost_MINOR_VERSION} EQUAL 65)
        target_compile_definitions(
            _pcraster_modflow_python
        PUBLIC
            -DBOOST_PYTHON_USE_GCC_SYMBOL_VISIBILITY)
    endif()
endif()

target_link_libraries(_pcraster_modflow_python
    pcraster::pcrmf
)

add_dependencies(_pcraster_modflow_python
    _pcraster fill_python_directory_structure
)

configure_python_extension(_pcraster_modflow_python
    "_pcraster_modflow"
)

set(_TARGET_BIN ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})

set_target_properties(_pcraster_modflow_python
    PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY "${_TARGET_BIN}/pcraster"
        RUNTIME_OUTPUT_DIRECTORY "${_TARGET_BIN}/pcraster"
)
