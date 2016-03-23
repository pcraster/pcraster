add_subdirectory(pcraster_manual)

if(PCRASTER_BUILD_DOCUMENTATION)
  add_subdirectory(python_modelling_framework)
  add_subdirectory(python_arrayed_variables)
endif()

# TODO add_subdirectory(link_out)
