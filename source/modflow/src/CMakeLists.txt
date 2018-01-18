add_library(pcrmf STATIC
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

add_library(pcraster::pcrmf ALIAS pcrmf)

target_include_directories(pcrmf
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        ${PYTHON_INCLUDE_DIR}
)

target_link_libraries(pcrmf
    PUBLIC
        pcraster::model_engine
        ${PCR_BOOST_PYTHON}
        ${PYTHON_LIBRARIES}
)

add_dependencies(pcrmf pcrcalc)
