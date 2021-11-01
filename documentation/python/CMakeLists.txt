set(SPHINX_SOURCES
    ../../data/demo/example.py
    differences.rst
    index.rst
    introduction.rst
    known_bugs.rst
    quickstart.rst
    reference.rst
)

prepare_pcraster_project_docs("${SPHINX_SOURCES}" "python" "PYTHON_SPHINX_SOURCES")
