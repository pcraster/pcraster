set(SPHINX_SOURCES
    applications.rst
    build.rst
    changes.rst
    # develop.rst
    # index.rst
    easybuild.rst
    install.rst
    macports.rst
    multicore.rst
    sources.rst
    pcraster_python_aguila_conda.png
    pcraster_index.rst
    pcraster_lue.rst
    pcraster_qgis.rst
    questions.rst
    QGIS_PCRaster_plugin.png
)

prepare_pcraster_project_docs("${SPHINX_SOURCES}" "pcraster_project" "PROJECT_SPHINX_SOURCES")
