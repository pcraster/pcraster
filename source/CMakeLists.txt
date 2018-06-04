configure_file(
    include/dev_Configure.h.in
    include/dev_Configure.h
)

if(PCRASTER_BUILD_DOCUMENTATION)
    include(DoxygenDoc)
endif()

add_subdirectory(rasterformat)

add_subdirectory(math_utils)
add_subdirectory(string_utils)
add_subdirectory(utils)
add_subdirectory(pcraster_dal)
add_subdirectory(pcraster_xsd)
add_subdirectory(app)
add_subdirectory(mathx)
add_subdirectory(curr)
add_subdirectory(misc)
add_subdirectory(pcraster_moc)
add_subdirectory(pcrblock)
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
add_subdirectory(pcraster_old_calc)
add_subdirectory(pcraster_python)
add_subdirectory(mldd)

add_subdirectory(python_modelling_framework)
add_subdirectory(python_arrayed_variables)

add_subdirectory(asc2map)
add_subdirectory(pcrcalc)
add_subdirectory(oldcalc)
add_subdirectory(col2map)
add_subdirectory(map2asc)
add_subdirectory(map2col)
add_subdirectory(mapattr)
add_subdirectory(resample)
add_subdirectory(table)
add_subdirectory(legend)

if(PCRASTER_WITH_AGUILA)
    add_subdirectory(pcraster_aguila)
endif()

if(PCRASTER_WITH_MODFLOW)
    add_subdirectory(modflow)
endif()

if(PCRASTER_WITH_PYTHON_MULTICORE)
    add_subdirectory(fern)
    add_subdirectory(pcraster_multicore)
endif()

if(PCRASTER_BUILD_BLOCKPYTHON)
    add_subdirectory(pcraster_block_python)
endif()


if(PCRASTER_BUILD_TEST)
    add_subdirectory(mapdiff)
endif()

install(
    FILES
        ${CMAKE_CURRENT_SOURCE_DIR}/include/pcrdll.h
    DESTINATION
        doc/developer/c/include
)
