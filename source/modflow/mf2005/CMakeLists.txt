# On Unices we build Modflow, on Windows use the USGS version
if(NOT WIN32)

    enable_language(Fortran)

    set(MODFLOW_FORTRAN_SOURCES
        utl7.f
        gwfsfrmodule.f
        gwf2swr7util.f
        gwf2bas7.f
        gwf2rch7.f
        gmg7_c_interface.f90
        sip7.f
        gwf2res7.f
        gwf2chd7.f
        gwf2swt7.f
        obs2bas7.f
        obs2chd7.f
        gwf2huf7.f
        gwf2ets7.f
        pcg7.f
        hufutl7.f
        gwf2str7.f
        de47.f
        gwf2ibs7.f
        gwf2bcf7.f
        pcgn_solve2.f90
        gwf2hfb7.f
        gwflakmodule.f
        gwfuzfmodule.f
        gwf2gag7.f
        gwf2evt7.f
        gwf2lpf7.f
        gwf2drn7.f
        gwf2fhb7.f
        mhc7.f
        gwf2sub7.f
        gwf2wel7.f
        gmg7.f
        gwf2drt7.f
        parutl7.f
        gwf2mnw27.f
        gwf2ghb7.f
        gwf2riv7.f
        gwf2hydmod7.f
        obs2str7.f
        gwf2mnw2i7.f
        pcgn2.f90
        gwf2sfr7.f
        gwf2swi27.f
        gwf2lak7.f
        gwf2swr7.f
        obs2riv7.f
        obs2drn7.f
        obs2ghb7.f
        gwf2uzf1.f
        gwf2mnw17.f
        mf2005.f
        lmt8.f
    )

    foreach(source in ${MODFLOW_FORTRAN_SOURCES})
      set_source_files_properties(${source} PROPERTIES
        COMPILE_FLAGS
          "-fbacktrace -ffpe-summary=overflow"
      )
    endforeach()

    set(MODFLOW_C_SOURCES
        r_vector.c
        mf2kgmg.c
        solvers.c
        ccfd.c
    )

    add_executable(mf2005
        ${MODFLOW_FORTRAN_SOURCES}
        ${MODFLOW_C_SOURCES}
    )

else()
    # Copy binary
    add_custom_target(mf2005
        COMMAND ${CMAKE_COMMAND}
            ARGS -E copy
                ${PROJECT_SOURCE_DIR}/external/Windows/64/mf2005.exe
                ${EXECUTABLE_OUTPUT_PATH}
    )

endif()


install(
    TARGETS
        mf2005
    DESTINATION bin COMPONENT Runtime
)
