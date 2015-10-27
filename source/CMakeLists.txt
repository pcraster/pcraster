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


add_subdirectory(math_utils)
add_subdirectory(string_utils)
add_subdirectory(utils)
add_subdirectory(pcraster_aguila)
add_subdirectory(pcraster_dal)
add_subdirectory(pcraster_xsd)


add_subdirectory(app)
add_subdirectory(mathx)
add_subdirectory(curr)
add_subdirectory(misc)
add_subdirectory(pcraster_moc)  # TODO unit tests
add_subdirectory(pcrblock)  # TODO unit tests
add_subdirectory(pcraster_block_python)  # TODO unit tests
add_subdirectory(pcrcom)
add_subdirectory(pcrdiscr)  # TODO unit tests
add_subdirectory(pcrfunc)  # TODO unit tests
add_subdirectory(pcrgeo)  # TODO unit tests
add_subdirectory(pcrgeom)
add_subdirectory(pcrraster)  # TODO unit tests
add_subdirectory(pcrvoxelstack)  # TODO unit tests

add_subdirectory(api)  # TODO unit tests
add_subdirectory(tab)
add_subdirectory(geom)  # TODO unit tests
add_subdirectory(calc)  # TODO unit tests
add_subdirectory(pccts)
add_subdirectory(pcrxml)  # TODO unit tests
add_subdirectory(modflow)  # TODO unit tests
add_subdirectory(linkinexamples)
add_subdirectory(pcraster_model_engine)  # TODO unit tests
add_subdirectory(pcraster_old_calc)  # TODO unit tests
add_subdirectory(pcraster_python)  # TODO unit tests
add_subdirectory(mldd)  # TODO unit tests

add_subdirectory(asc2map)
add_subdirectory(pcrcalc)
add_subdirectory(oldcalc)
add_subdirectory(col2map)
add_subdirectory(map2asc)
add_subdirectory(map2col)
add_subdirectory(mapattr)
add_subdirectory(resample)  # TODO unit tests
add_subdirectory(table)
add_subdirectory(legend)

# Used in unit tests.
add_subdirectory(mapdiff)


install(
    FILES
        ${CMAKE_CURRENT_SOURCE_DIR}/include/pcrdll.h
    DESTINATION
        doc/developer/c/include
)
