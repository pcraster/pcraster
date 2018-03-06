set(SPHINX_SOURCES
    ../../data/modflow/demo/example.mod
    ../../data/modflow/demo/example.py
    bas.rst
    bcf.rst
    dis.rst
    drn.rst
    example.rst
    index.rst
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
    modflow2005.rst
)

prepare_pcraster_project_docs("${SPHINX_SOURCES}" "modflow" "MODFLOW_SPHINX_SOURCES")

create_sphinx_docs(
    TARGET pcraster_modflow_manual
    SOURCES ${SPHINX_SOURCES}
)

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_build/html/
    DESTINATION doc/modflow
)
