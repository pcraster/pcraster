set(SPHINX_SOURCES
    build.rst
    changes.rst
    # develop.rst
    # index.rst
    install.rst
    multicore.rst
    sources.rst
    pcraster_python_aguila_conda.png
    pcraster_index.rst
    questions.rst
)

prepare_pcraster_project_docs("${SPHINX_SOURCES}" "pcraster_project" "PROJECT_SPHINX_SOURCES")
