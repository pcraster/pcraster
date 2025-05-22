# NO_EXTRAS required for success on macOS conda build
pybind11_add_module(_pcraster_modflow_python MODULE NO_EXTRAS THIN_LTO
    pcrmfpy.cc
)

add_library(pcraster::modflow_python ALIAS _pcraster_modflow_python)

target_include_directories(_pcraster_modflow_python
    PRIVATE
        $<TARGET_PROPERTY:pcraster::pcrmf_python,INTERFACE_INCLUDE_DIRECTORIES>
)

target_link_libraries(_pcraster_modflow_python
    PUBLIC
        pcraster::pcrmf_python
)

set_target_properties(_pcraster_modflow_python
    PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster
        LIBRARY_OUTPUT_DIRECTORY
            $<TARGET_FILE_DIR:pcrcalc>/pcraster
        OUTPUT_NAME
            "_pcraster_modflow"
        INSTALL_RPATH
            "$ORIGIN/../../lib"
        DEBUG_POSTFIX
            ""
        PREFIX
            "${PYTHON_MODULE_PREFIX}"
        SUFFIX
            "${PYTHON_MODULE_EXTENSION}"
)

add_dependencies(_pcraster_modflow_python
    _pcraster
    pcraster_modflow
)

#----------------------------------------------------------
install(
    TARGETS _pcraster_modflow_python
    LIBRARY DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster
    RUNTIME DESTINATION ${PCRASTER_PYTHON_INSTALL_DIR}/pcraster
)