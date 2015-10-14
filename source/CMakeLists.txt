configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/dev_Configure.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/include/dev_Configure.h
)
include_directories(
    ${CMAKE_CURRENT_BINARY_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)


if(PCRASTER_BUILD_DOCUMENTATION)
    include(DoxygenDoc)
endif()


add_subdirectory(math_utils)  # OK
add_subdirectory(string_utils)  # OK
add_subdirectory(utils)  # OK
### add_subdirectory(pcraster_aguila)
add_subdirectory(pcraster_dal)  # OK
### add_subdirectory(pcraster_xsd)


add_subdirectory(app)  # OK
add_subdirectory(mathx)  # OK
add_subdirectory(curr)  # OK
add_subdirectory(misc)  # OK
### add_subdirectory(pcraster_moc)
### add_subdirectory(pcrblock)
### add_subdirectory(pcraster_block_python)
add_subdirectory(pcrcom)  # OK
### add_subdirectory(pcrdiscr)
### add_subdirectory(pcrfunc)
### add_subdirectory(pcrgeo)
### add_subdirectory(pcrgeom)
### add_subdirectory(pcrraster)
### add_subdirectory(pcrvoxelstack)
### 
### add_subdirectory(api)
add_subdirectory(tab)  # OK
### add_subdirectory(geom)
### add_subdirectory(calc)
### add_subdirectory(pccts)
### add_subdirectory(pcrxml)
### add_subdirectory(modflow)
### add_subdirectory(linkinexamples)
### add_subdirectory(pcraster_model_engine)
### add_subdirectory(pcraster_old_calc)
### add_subdirectory(pcraster_python)
### add_subdirectory(mldd)
### 
add_subdirectory(asc2map)
### add_subdirectory(pcrcalc)
### add_subdirectory(oldcalc)
add_subdirectory(col2map)
add_subdirectory(map2asc)
add_subdirectory(map2col)
add_subdirectory(mapattr)
### add_subdirectory(resample)
### add_subdirectory(table)
### add_subdirectory(legend)
### 
### # Used in unit tests.
### add_subdirectory(mapdiff)


install(
    FILES
        ${CMAKE_CURRENT_SOURCE_DIR}/include/pcrdll.h
    DESTINATION
        doc/developer/c/include
)
