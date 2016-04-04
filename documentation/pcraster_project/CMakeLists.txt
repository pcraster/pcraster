set(SPHINX_SOURCES
    build.rst
    changes.rst
    develop.rst
    index.rst
    install.rst
)

create_sphinx_docs(
    TARGET pcraster_project_documentation
    SOURCES ${SPHINX_SOURCES}
)

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_build/html/
    DESTINATION doc/pcraster
)
