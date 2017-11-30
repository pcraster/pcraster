include_directories(
    # ${CMAKE_CURRENT_SOURCE_DIR}/../../include
     ${CMAKE_CURRENT_SOURCE_DIR}/../src
     ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrdiscr
     ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_dal
     ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_model_engine
     ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrgeo
)

SET(SOURCES
    pcrmfpy.cc
)
IF(CMAKE_COMPILER_IS_GNUCXX)
    SET_SOURCE_FILES_PROPERTIES(pcrmfpy.cc
        COMPILE_FLAGS
            "-Wno-strict-aliasing"
    )
ENDIF()

ADD_LIBRARY(_pcraster_modflow_python
    SHARED
    ${SOURCES}
)
if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    # otherwise compilation with hidden visibility flags will fail
    if(${Boost_MINOR_VERSION} EQUAL 65)
        target_compile_definitions(
            _pcraster_modflow_python
        PUBLIC
            -DBOOST_PYTHON_USE_GCC_SYMBOL_VISIBILITY)
    endif()
endif()
TARGET_LINK_LIBRARIES(_pcraster_modflow_python
     pcrmf
     pcrgeo
#     pcrblock
     pcraster_model_engine
#     pcrcom
#     pcrdiscr
#    ${Boost_PYTHON_LIBRARY}
    ${PYTHON_LIBRARIES}
    # ${PCRASTER_RASTER_FORMAT}
    # ${GDAL_LIBRARIES}
    # ${BOOST_SIGNALS_LIBRARY}
    # ${BOOST_THREAD_LIBRARY}
#    Boost::python
    ${PCR_BOOST_PYTHON}
)

add_dependencies(_pcraster_modflow_python
    _pcraster fill_python_directory_structure
)

CONFIGURE_PYTHON_EXTENSION(_pcraster_modflow_python
    "_pcraster_modflow"
)

set(_TARGET_BIN ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})

set_target_properties(_pcraster_modflow_python
    PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY "${_TARGET_BIN}/pcraster"
        RUNTIME_OUTPUT_DIRECTORY "${_TARGET_BIN}/pcraster"
)


