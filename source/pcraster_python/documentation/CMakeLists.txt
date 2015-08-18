SET(SPHINX_SOURCES
    ../../../data/demo/example.py
    differences.rst
    index.rst
    introduction.rst
    known_bugs.rst
    quickstart.rst
)
SET(SPHINX_TARGET pcraster_python_manual)
INCLUDE(SphinxDoc)
INSTALL(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_build/html/
    DESTINATION doc/python/pcraster
)
