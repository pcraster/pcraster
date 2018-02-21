# pcraster_manual required to run unit tests
add_subdirectory(pcraster_manual)


if(PCRASTER_BUILD_DOCUMENTATION)
    # add_subdirectory(python_modelling_framework)
    # add_subdirectory(python_arrayed_variables)
    # add_subdirectory(pcraster_aguila)
    add_subdirectory(pcraster_project)
    # add_subdirectory(link_out)
    # add_subdirectory(modflow)
    # add_subdirectory(python)
endif()
