configure_file(
    include/dev_Configure.h.in
    include/dev_Configure.h
)

configure_file(
    include/pcraster_version.h.in
    include/pcraster_version.h
)

if(WIN32)
    add_subdirectory(pdcurses EXCLUDE_FROM_ALL)
endif()
add_subdirectory(math_utils)
add_subdirectory(string_utils)
add_subdirectory(utils)
add_subdirectory(pcraster_dal)
add_subdirectory(pcraster_xsd)
add_subdirectory(app)
add_subdirectory(mathx)
add_subdirectory(curr)
add_subdirectory(misc)
add_subdirectory(pcrcom)
add_subdirectory(pcrdiscr)
add_subdirectory(pcrfunc)
add_subdirectory(pcrgeo)
add_subdirectory(pcrgeom)
add_subdirectory(pcrraster)
add_subdirectory(pcrvoxelstack)
add_subdirectory(api)
add_subdirectory(tab)
add_subdirectory(geom)
add_subdirectory(calc)
add_subdirectory(pccts)
add_subdirectory(pcrxml)
add_subdirectory(linkinexamples)
add_subdirectory(pcraster_model_engine)
add_subdirectory(pcraster_python)

add_subdirectory(python_modelling_framework)
add_subdirectory(python_arrayed_variables)

add_subdirectory(asc2map)
add_subdirectory(pcrcalc)
add_subdirectory(col2map)
add_subdirectory(map2asc)
add_subdirectory(map2col)
add_subdirectory(mapattr)
add_subdirectory(resample)
add_subdirectory(table)
add_subdirectory(legend)

if(PCRASTER_BUILD_AGUILA)
    add_subdirectory(pcraster_aguila)
endif()

if(PCRASTER_BUILD_MODFLOW)
    add_subdirectory(modflow)
endif()

if(PCRASTER_BUILD_OLDCALC)
    add_subdirectory(oldcalc)
    add_subdirectory(pcraster_old_calc)
endif()

if(PCRASTER_BUILD_TEST)
    add_subdirectory(mapdiff)
endif()

if(PCRASTER_BUILD_DOCUMENTATION)
    include(DoxygenDoc)
endif()

if(PCRASTER_BUILD_BLOCKPYTHON)
    add_subdirectory(pcrblock)
    add_subdirectory(pcraster_block_python)
endif()

if(PCRASTER_BUILD_MOC)
    add_subdirectory(pcraster_moc)
endif()

if(PCRASTER_BUILD_MLDD)
    add_subdirectory(mldd)
endif()

# Add subprojects last as they reset CMake policies
if(PCRASTER_BUILD_MULTICORE)
    add_subdirectory(pcraster_multicore)
endif()


if(PCRASTER_INSTALL_FILES_DEVELOPER)
    install(
        FILES
            ${CMAKE_CURRENT_SOURCE_DIR}/include/pcrdll.h
        DESTINATION
            doc/developer/c/include
    )
endif()
