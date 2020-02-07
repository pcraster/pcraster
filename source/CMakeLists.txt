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
    set(FERN_BUILD_ALGORITHM ON CACHE BOOL "Build submodule fern")
    # For development/travis build the tests to catch errors early
    if(PCRASTER_BUILD_TEST_FERN)
      add_subdirectory(fern)
    else()
      # Default/when packaging PCRaster, do not install anything from Fern
      set(DEVBASE_BUILD_TEST OFF CACHE BOOL "Disable Fern tests")
      add_subdirectory(fern EXCLUDE_FROM_ALL)
    endif()
    add_subdirectory(pcraster_multicore)
endif()

add_subdirectory(rasterformat EXCLUDE_FROM_ALL)


if(PCRASTER_INSTALL_FILES_DEVELOPER)
    install(
        FILES
            ${CMAKE_CURRENT_SOURCE_DIR}/include/pcrdll.h
        DESTINATION
            doc/developer/c/include
    )
endif()

# Place 3rd party libraries in our lib directory to create a
# 'bundle'
# fixup_bundle doesn't work out of the box (on Linux), do some steps manually.
#
# From here on it's getting ugly, but just ram it in fttb...
#
# gdal might pull the entire world, clean that up at some point


function(get_dependent_so var arg)
  # Try to obtain all *so* files
  get_target_property(DEP_SO ${arg} LOCATION)
  get_filename_component(DEP_SO ${DEP_SO} REALPATH)
  get_filename_component(DEP_SO_DIR ${DEP_SO} DIRECTORY)
  get_filename_component(DEP_SO_NAME ${DEP_SO} NAME_WE ABSOLUTE)

  file(GLOB ALL_SO "${DEP_SO_DIR}/${DEP_SO_NAME}*${CMAKE_SHARED_LIBRARY_SUFFIX}*")

  set (var ${ALL_SO} PARENT_SCOPE)
endfunction()



set(LIBS_PATH "")


if(PCRASTER_PACKAGE_BOOST)

  get_dependent_so(var Boost::filesystem)
  list(APPEND LIBS_PATH ${var})

  get_dependent_so(var Boost::date_time)
  list(APPEND LIBS_PATH ${var})

  get_dependent_so(var Boost::program_options)
  list(APPEND LIBS_PATH ${var})

endif()



if(PCRASTER_PACKAGE_XERCES)

  get_dependent_so(var XercesC::XercesC)
  list(APPEND LIBS_PATH ${var})

endif()

if(PCRASTER_PACKAGE_GDAL)
  get_dependent_so(var GDAL::GDAL)
  list(APPEND LIBS_PATH ${var})
endif()



install(CODE "
  set(LIBS_PATH \"${LIBS_PATH}\")

  include(GetPrerequisites)


  foreach(lib \${LIBS_PATH})

    get_filename_component(FILENAME \${lib} NAME)
    message(STATUS \"    Installing \" \${lib})
    file(COPY \${lib} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)
    execute_process(COMMAND \"patchelf\" \"--set-rpath\" \"\$ORIGIN/.\" \"\${CMAKE_INSTALL_PREFIX}/lib/\${FILENAME}\" ERROR_QUIET)


    get_prerequisites(\${lib} FIX_SO_LIBRARIES 0 1 \"\" \"\" )

    list(FILTER FIX_SO_LIBRARIES INCLUDE REGEX \"boost|xerces|icu|double-conversion|libz|gthread|glib|pcre|iconv|hdf|kea|pcre|spatial|dap|proj|png|tiff|gif|cdf|poppler|kml|jpeg|libpq|cfitsio|freexl|json-c|tiledb|crypto|geos|expat|curl\")


    foreach(dep \${FIX_SO_LIBRARIES})
      message(STATUS \"       Installing \" \${dep})

      file(GLOB ALL_SO2 \"\${dep}*\")

      foreach(dep2 \${ALL_SO2})

        file(COPY \${dep2} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)

        get_filename_component(FILENAME \${dep2} NAME)
        execute_process(COMMAND \"patchelf\" \"--set-rpath\" \"\$ORIGIN/.\" \"\${CMAKE_INSTALL_PREFIX}/lib/\${FILENAME}\" ERROR_QUIET)

      endforeach()

    endforeach()

  endforeach()


" COMPONENT Runtime)



# Do not tweak RPATH with Qt libraries, or you'll suffer endless pain
# Just copy them

set(LIBS_PATH "")

if(PCRASTER_PACKAGE_QT)

  get_dependent_so(var Qt5::Core)
  list(APPEND LIBS_PATH ${var})

  get_dependent_so(var Qt5::Sql)
  list(APPEND LIBS_PATH ${var})

  get_dependent_so(var Qt5::Xml)
  list(APPEND LIBS_PATH ${var})

  if(PCRASTER_BUILD_AGUILA)
    get_dependent_so(var Qt5::Gui)
    list(APPEND LIBS_PATH ${var})

    get_dependent_so(var Qt5::Widgets)
    list(APPEND LIBS_PATH ${var})

    get_dependent_so(var Qt5::Charts)
    list(APPEND LIBS_PATH ${var})
  endif()

  if(PCRASTER_WITH_OPENGL)
    get_dependent_so(var Qt5::OpenGL)
    list(APPEND LIBS_PATH ${var})
  endif()

endif()


if(PCRASTER_PACKAGE_QT_PLATFORMS)
  get_target_property(QT_PLATFORM_DIR Qt5::Core LOCATION)
  get_filename_component(QT_PLATFORM_DIR ${QT_PLATFORM_DIR} DIRECTORY)
  install(DIRECTORY ${QT_PLATFORM_DIR}/../plugins/platforms DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
endif()



install(CODE "
  set(LIBS_PATH \"${LIBS_PATH}\")

  include(GetPrerequisites)

  foreach(lib \${LIBS_PATH})

    get_filename_component(FILENAME \${lib} NAME)
    message(STATUS \"    Installing \" \${lib})
    file(COPY \${lib} DESTINATION \${CMAKE_INSTALL_PREFIX}/lib)

  endforeach()
" COMPONENT Runtime)
