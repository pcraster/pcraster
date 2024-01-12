add_library(pcrmf_objects OBJECT
    mf_BinaryReader.cc
    mf_utils.cc
    bas.cc
    bcf.cc
    common.cc
    dis.cc
    drn.cc
    dsp.cc
    ghb.cc
    gridcheck.cc
   #  hfb
    pcg.cc
    rch.cc
    riv.cc
    sip.cc
    sor.cc
    wel.cc
    pcrmodflow.cc
)

# Add flags required for Qt 6
if ((MSVC) AND (MSVC_VERSION GREATER_EQUAL 1914))
    target_compile_options(pcrmf_objects PRIVATE "/Zc:__cplusplus" "/permissive-")
endif()

target_compile_definitions(pcrmf_objects
    PRIVATE
        PCR_DAL_SHARED_LINK
)

target_include_directories(pcrmf_objects
    PRIVATE
        $<TARGET_PROPERTY:pcraster::model_engine,INTERFACE_INCLUDE_DIRECTORIES>
)

target_include_directories(pcrmf_objects SYSTEM
    PRIVATE
        ${Boost_INCLUDE_DIRS}
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
    mf_ModflowPython.cc
    $<TARGET_OBJECTS:pcrmf_objects>
)

add_library(pcraster::pcrmf_python ALIAS pcrmfpy)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    # It seems that clang doesn't set flag this by default yet
    set_property(
        SOURCE
            mf_ModflowPython.cc
        PROPERTY
            COMPILE_OPTIONS
                -fsized-deallocation
    )
endif()


target_include_directories(pcrmfpy
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        ${pybind11_SOURCE_DIR}/include
        ${Python_INCLUDE_DIRS}
)

target_link_libraries(pcrmfpy
    PUBLIC
        pcraster::model_engine
)
