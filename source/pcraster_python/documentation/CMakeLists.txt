set(SPHINX_SOURCES
    ../../../data/demo/example.py
    differences.rst
    index.rst
    introduction.rst
    known_bugs.rst
    quickstart.rst
)

create_sphinx_docs(
    TARGET pcraster_python_manual
    SOURCES ${SPHINX_SOURCES}
)

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_build/html/
    DESTINATION doc/python/pcraster
)
