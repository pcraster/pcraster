add_library(pcraster_python_utils SHARED
    ppu_exception.cc
    Globals.cc
)

add_library(pcraster::python::utils ALIAS pcraster_python_utils)

set_target_properties(pcraster_python_utils
    PROPERTIES
        SOVERSION ${${PROJECT_NAME}_MAJOR_VERSION}
        VERSION ${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}
)

generate_export_header(pcraster_python_utils
    BASE_NAME pcraster_python_utils
)

target_include_directories(pcraster_python_utils
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcraster_python_utils
    PUBLIC
        pcraster::model_engine
)

install(
    TARGETS
        pcraster_python_utils
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
    LIBRARY
        DESTINATION ${PCRASTER_LIB_INSTALL_DIR} COMPONENT Runtime
)
