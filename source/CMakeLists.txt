configure_file(
    include/dev_Configure.h.in
    include/dev_Configure.h
)

configure_file(
    include/pcraster_version.h.in
    include/pcraster_version.h
)

if(WIN32)
    add_subdirectory(pdcurses)
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
    set(FERN_BUILD_ALGORITHM ON CACHE BOOL "Build submodule fern")
    add_subdirectory(fern)
    add_subdirectory(pcraster_multicore)
endif()

add_subdirectory(rasterformat)


install(
    FILES
        ${CMAKE_CURRENT_SOURCE_DIR}/include/pcrdll.h
    DESTINATION
        doc/developer/c/include
)


# Place 3rd party libraries in our lib directory to create a
# 'bundle'
# fixup_bundle doesn't work out of the box (on Linux), do some steps manually.
#
# From here on it's getting ugly, but just ram it in fttb...
#
# gdal might pull the entire world, clean that up at some point

if(PCRASTER_PACKAGE_BOOST)

  install(CODE "
    include(GetPrerequisites)

    # Get dependencies from some executables
    get_prerequisites(\${CMAKE_INSTALL_PREFIX}/bin/pcrcalc FIX_SO_LIBRARIES 0 1 \"\" \"\" )
    get_prerequisites(\${CMAKE_INSTALL_PREFIX}/bin/aguila  FIX_SO_LIBRARIES 0 1 \"\" \"\" )
    get_prerequisites(\${CMAKE_INSTALL_PREFIX}/bin/oldcalc  FIX_SO_LIBRARIES 0 1 \"\" \"\" )
    get_prerequisites(\${CMAKE_INSTALL_PREFIX}/bin/resample  FIX_SO_LIBRARIES 0 1 \"\" \"\" )

    list(FILTER FIX_SO_LIBRARIES INCLUDE REGEX \"boost\")

    foreach(lib \${FIX_SO_LIBRARIES})
      file(COPY \${lib} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
      message(STATUS \"Packaging \" \${lib})
      get_filename_component(ABS_DIR \${lib} REALPATH)
      file(COPY \${ABS_DIR} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
      message(STATUS \"Packaging \" \${ABS_DIR})
      # Fix the rpath to our lib directory
      get_filename_component(FILENAME \${lib} NAME)

      # Using the \$ORIGIN/../lib as we did before will fail with a conda build, this should be fine...
      execute_process(COMMAND \"patchelf\" \"--set-rpath\" \"\$ORIGIN/.\" \"\${CMAKE_INSTALL_PREFIX}/lib/\${FILENAME}\")
    endforeach()
  " COMPONENT Runtime)

endif()


if(PCRASTER_PACKAGE_NCURSES)
  install(CODE "
    include(GetPrerequisites)

    get_prerequisites(\${CMAKE_INSTALL_PREFIX}/bin/mapattr FIX_SO_LIBRARIES 0 1 \"\" \"\" )

    list(FILTER FIX_SO_LIBRARIES INCLUDE REGEX \"curses|tinfo\")

    foreach(lib \${FIX_SO_LIBRARIES})
      file(COPY \${lib} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
      message(STATUS \"Packaging \" \${lib})
      get_filename_component(ABS_DIR \${lib} REALPATH)
      file(COPY \${ABS_DIR} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
      message(STATUS \"Packaging \" \${ABS_DIR})
      # Fix the rpath to our lib directory
      get_filename_component(FILENAME \${lib} NAME)
      execute_process(COMMAND \"patchelf\" \"--set-rpath\" \"\$ORIGIN/.\" \"\${CMAKE_INSTALL_PREFIX}/lib/\${FILENAME}\")
    endforeach()
  " COMPONENT Runtime)

endif()


if(PCRASTER_PACKAGE_XERCES)

  install(CODE "
    include(GetPrerequisites)

    get_prerequisites(\${CMAKE_INSTALL_PREFIX}/bin/pcrcalc FIX_SO_LIBRARIES 0 1 \"\" \"\" )

    list(FILTER FIX_SO_LIBRARIES INCLUDE REGEX \"xerces|icu\")

    foreach(lib \${FIX_SO_LIBRARIES})
      file(COPY \${lib} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
      message(STATUS \"Packaging \" \${lib})
      get_filename_component(ABS_DIR \${lib} REALPATH)
      file(COPY \${ABS_DIR} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
      message(STATUS \"Packaging \" \${ABS_DIR})
      # Fix the rpath to our lib directory
      get_filename_component(FILENAME \${lib} NAME)
      execute_process(COMMAND \"patchelf\" \"--set-rpath\" \"\$ORIGIN/.\" \"\${CMAKE_INSTALL_PREFIX}/lib/\${FILENAME}\")

    endforeach()
  " COMPONENT Runtime)

endif()


if(PCRASTER_PACKAGE_QT)

  install(CODE "
    include(GetPrerequisites)

    get_prerequisites(\${CMAKE_INSTALL_PREFIX}/bin/pcrcalc FIX_SO_LIBRARIES 0 1 \"\" \"\" )
    get_prerequisites(\${CMAKE_INSTALL_PREFIX}/bin/aguila FIX_SO_LIBRARIES 0 1 \"\" \"\" )
    get_prerequisites(\${CMAKE_INSTALL_PREFIX}/bin/oldcalc FIX_SO_LIBRARIES 0 1 \"\" \"\" )

    list(FILTER FIX_SO_LIBRARIES INCLUDE REGEX \"Qt5|double-conversion|libz|gthread|glib|pcre|iconv\")

    foreach(lib \${FIX_SO_LIBRARIES})
      file(COPY \${lib} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
      message(STATUS \"Packaging \" \${lib})
      get_filename_component(ABS_DIR \${lib} REALPATH)
      file(COPY \${ABS_DIR} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
      message(STATUS \"Packaging \" \${ABS_DIR})
      # Fix the rpath to our lib directory
      get_filename_component(FILENAME \${lib} NAME)
      execute_process(COMMAND \"patchelf\" \"--set-rpath\" \"\$ORIGIN/.\" \"\${CMAKE_INSTALL_PREFIX}/lib/\${FILENAME}\")
    endforeach()
  " COMPONENT Runtime)

endif()


if(PCRASTER_PACKAGE_GDAL)

  install(CODE "
    include(GetPrerequisites)

    get_prerequisites(\${CMAKE_INSTALL_PREFIX}/bin/pcrcalc FIX_SO_LIBRARIES 0 1 \"\" \"\" )

    list(FILTER FIX_SO_LIBRARIES INCLUDE REGEX \$ENV{BUILD_PREFIX})

    foreach(lib \${FIX_SO_LIBRARIES})
      file(COPY \${lib} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
      message(STATUS \"Packaging \" \${lib})
      get_filename_component(ABS_DIR \${lib} REALPATH)
      file(COPY \${ABS_DIR} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
      message(STATUS \"Packaging \" \${ABS_DIR})
      # Fix the rpath to our lib directory
      get_filename_component(FILENAME \${lib} NAME)
      execute_process(COMMAND \"patchelf\" \"--set-rpath\" \"\$ORIGIN/.\" \"\${CMAKE_INSTALL_PREFIX}/lib/\${FILENAME}\")
    endforeach()
  " COMPONENT Runtime)

endif()




