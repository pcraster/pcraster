set(SPHINX_SOURCES
    ../demo/example.mod
    ../demo/example.py
    bas.rst
    bcf.rst
    dis.rst
    drn.rst
    example.rst
    index.rst
    introduction.rst
    layerdesc.png
    mfin.rst
    mfout.rst
    quickstart.rst
    rch.rst
    riv.rst
    solver.rst
    usage.rst
    usagepy.rst
    wel.rst
    modflow2000.rst
)

create_sphinx_docs(
    TARGET pcraster_modflow_manual
    SOURCES ${SPHINX_SOURCES}
)

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_build/html/
    DESTINATION doc/modflow
)
