if(CMAKE_COMPILER_IS_GNUCXX)
    SET_SOURCE_FILES_PROPERTIES(pcrmfpy.cc
        COMPILE_FLAGS
            "-Wno-strict-aliasing"
    )
endif()

pybind11_add_module(_pcraster_modflow_python MODULE
    pcrmfpy.cc
)

target_include_directories(_pcraster_modflow_python
    PRIVATE
        $<TARGET_PROPERTY:pcraster::pcrmf_python,INTERFACE_INCLUDE_DIRECTORIES>
)

add_library(pcraster::modflow_python ALIAS _pcraster_modflow_python)

target_link_libraries(_pcraster_modflow_python
  PUBLIC
    pcraster::pcrmf_python
    pybind11::module
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
