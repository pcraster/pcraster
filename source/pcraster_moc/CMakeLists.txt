# TODO Create Documentation, PCRCalc, Python, ... subdirectories.
add_subdirectory(python)

add_library(pcraster_moc_objects OBJECT
    pt_MOCLink
    pt_ModelLinks
    pt_Particle
    pt_ParticleTracker
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

install(
    TARGETS
        pcraster_moc
    RUNTIME
        DESTINATION lib COMPONENT Runtime
    LIBRARY
        DESTINATION lib COMPONENT Runtime
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE moc
        NAMES
            pt_ParticleTest
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
