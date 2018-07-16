# Helper macro to return paths to rst files such that
# the project documenation target can access them
macro(prepare_pcraster_project_docs sources dirname output)
    foreach(item ${sources})
        set(${output} ${${output}} ${dirname}/${item})
    endforeach()
    set(${output} ${${output}} PARENT_SCOPE)
endmacro()

# pcraster_manual required to run unit tests
add_subdirectory(pcraster_manual)


if(PCRASTER_BUILD_DOCUMENTATION)
     add_subdirectory(link_out)
     add_subdirectory(modflow)
     add_subdirectory(pcraster_aguila)
     add_subdirectory(pcraster_project)
     add_subdirectory(python)
     add_subdirectory(python_arrayed_variables)
     add_subdirectory(python_modelling_framework)

    set(ALL_SPHINX_SOURCES
        ${ARRAYED_VARIABLES_SPHINX_SOURCES}
        ${MODELLING_FRAMEWORK_SPHINX_SOURCES}
        ${PYTHON_SPHINX_SOURCES}
        ${MODFLOW_SPHINX_SOURCES}
        ${AGUILA_SPHINX_SOURCES}
        ${PCRASTER_SPHINX_SOURCES}
        ${PROJECT_SPHINX_SOURCES}
        index.rst
    )

    create_sphinx_docs(
        TARGET pcraster_documentation
        SOURCES ${ALL_SPHINX_SOURCES}
    )

    #
    # add_dependencies(
    #    pcraster_documentation
    #
    # )

endif()
