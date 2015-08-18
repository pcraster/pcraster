include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrdiscr
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_model_engine
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../../utils
)

SET(SOURCES
    mf_Modflow.cc
)
SET(TEST_SOURCES
)
SET(OLDSOURCES
    mf_BinaryReader.cc
    bas.cc
    bcf.cc
    common.cc
    dis.cc
    drn.cc
    dsp.cc
    gridcheck.cc
    hfb.cc
    pcg.cc
    pcrmodflow.cc
    rch.cc
    riv.cc
    sip.cc
    sor.cc
    wel.cc
)
ADD_LIBRARY(pcrmf
    STATIC
    ${OLDSOURCES}
)

ADD_DEPENDENCIES(pcrmf pcrcalc)
