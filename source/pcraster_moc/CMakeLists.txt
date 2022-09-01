# TODO Create Documentation, PCRCalc, Python, ... subdirectories.
add_subdirectory(python)

add_library(pcraster_moc_objects OBJECT
    pt_MOCLink.cc
    pt_ModelLinks.cc
    pt_Particle.cc
    pt_ParticleTracker.cc
)

target_include_directories(pcraster_moc_objects
    PRIVATE
        $<TARGET_PROPERTY:pcraster::old_calc,INTERFACE_INCLUDE_DIRECTORIES>
)

target_include_directories(pcraster_moc_objects
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

add_library(pcraster_moc SHARED
    $<TARGET_OBJECTS:pcraster_moc_objects>
)

add_library(pcraster::moc ALIAS pcraster_moc)

target_include_directories(pcraster_moc
    PUBLIC
        $<TARGET_PROPERTY:pcraster_moc_objects,INTERFACE_INCLUDE_DIRECTORIES>
)

target_link_libraries(pcraster_moc
    PRIVATE
        pcraster::old_calc
)

set_target_properties(pcraster_moc
    PROPERTIES
        SOVERSION ${${PROJECT_NAME}_MAJOR_VERSION}
        VERSION ${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}
)

install(
    TARGETS
        pcraster_moc
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
    LIBRARY
        DESTINATION ${PCRASTER_LIB_INSTALL_DIR} COMPONENT Runtime
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE moc
        NAMES
            pt_ParticleTest.cc
        INCLUDE_DIRS
            $<TARGET_PROPERTY:pcraster_moc_objects,INTERFACE_INCLUDE_DIRECTORIES>
        OBJECT_LIBRARIES
            $<TARGET_OBJECTS:pcraster_moc_objects>
        LINK_LIBRARIES
            $<TARGET_PROPERTY:pcraster_moc,LINK_LIBRARIES>
    )
endif()

# run:
# 	runapp pcrcalc -f track.pcr
#
# debug.map: out dem.map
# 	runapp col2map --unitcell --clone dem.map -B out debug.map
#
# flux.map: flux.tbl dem.map
# 	runapp pcrcalc "flux.map=lookupscalar(flux.tbl, dem.map)"
# 	runapp pcrcalc "flux.map=if(defined(dem.map) and not(defined(flux.map)), 0.0, flux.map)"
