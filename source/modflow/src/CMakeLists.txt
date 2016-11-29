set(OLDSOURCES
    mf_BinaryReader.cc
    mf_utils.cc
    bas.cc
    bcf.cc
    common.cc
    dis.cc
    drn.cc
    dsp.cc
    gridcheck.cc
    hfb.cc
    pcg.cc
    rch.cc
    riv.cc
    sip.cc
    sor.cc
    wel.cc
    pcrmodflow.cc
)

add_library(pcrmf
    STATIC
    ${OLDSOURCES}
)

target_include_directories(pcrmf
  PRIVATE
#    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrdiscr
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcraster_model_engine
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pcrgeo
    ${CMAKE_CURRENT_SOURCE_DIR}/../../utils
)

target_link_libraries(pcrmf
    ${Boost_PYTHON_LIBRARY}
    ${PYTHON_LIBRARIES}
    Qt5::Core
)

add_dependencies(pcrmf pcrcalc)
