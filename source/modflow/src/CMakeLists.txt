add_library(pcrmf_objects OBJECT
    mf_BinaryReader
    mf_utils
    bas
    bcf
    common
    dis
    drn
    dsp
    gridcheck
   #  hfb
    pcg
    rch
    riv
    sip
    sor
    wel
    pcrmodflow
)

target_include_directories(pcrmf_objects
    PRIVATE
        $<TARGET_PROPERTY:pcraster::model_engine,INTERFACE_INCLUDE_DIRECTORIES>
)

add_library(pcrmf STATIC
    $<TARGET_OBJECTS:pcrmf_objects>
)

add_library(pcraster::pcrmf_calc ALIAS pcrmf)

target_include_directories(pcrmf
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcrmf
    PUBLIC
        pcraster::model_engine
)

add_dependencies(pcrmf pcrcalc)

add_library(pcrmfpy STATIC
    mf_ModflowPython
    $<TARGET_OBJECTS:pcrmf_objects>
)

add_library(pcraster::pcrmf_python ALIAS pcrmfpy)

target_include_directories(pcrmfpy
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcrmfpy
    PUBLIC
        pcraster::model_engine
        pybind11::module
)
