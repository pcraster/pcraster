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

create_sphinx_docs(
    TARGET python_arrayed_variables_documentation
    SOURCES ${SPHINX_SOURCES}
)

add_dependencies(python_arrayed_variables_documentation
    python_arrayed_variables_dummy
    fill_python_directory_structure
)

install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/_build/html/
    DESTINATION doc/python/pcraster/arrayed_variables
)
