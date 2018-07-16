set(SPHINX_SOURCES
    # build.rst
    changes.rst
    # develop.rst
    # index.rst
    install.rst
    multicore.rst
    sources.rst
)

prepare_pcraster_project_docs("${SPHINX_SOURCES}" "pcraster_project" "PROJECT_SPHINX_SOURCES")

create_sphinx_docs(
    TARGET pcraster_project_documentation
    SOURCES ${SPHINX_SOURCES}
)

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_build/html/
    DESTINATION doc/pcraster
)
