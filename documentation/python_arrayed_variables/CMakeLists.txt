set(SPHINX_SOURCES
    index.rst
    examples/index.rst
    reference/ClassIndex.rst
    reference/index.rst
    reference/VariableCollection.rst
    tutorial/index.rst
    ../../data/demo/python_arrayed_variables/parameterFile.py
    ../../data/demo/python_arrayed_variables/plant.tbl
    ../../data/demo/python_arrayed_variables/simpleLoops.py
)

prepare_pcraster_project_docs("${SPHINX_SOURCES}" "python_arrayed_variables" "ARRAYED_VARIABLES_SPHINX_SOURCES")

