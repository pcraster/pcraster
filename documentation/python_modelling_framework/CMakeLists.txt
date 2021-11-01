set(SPHINX_SOURCES
    Bibliography.rst
    Introduction.rst
    Manual.rst
    ModellingFrameworks.rst
    PCRasterPythonFramework.rst
    # TodoList.rst
    index.rst
    # some demo example scripts included in the documentation
    montecarloStatic.py
    ../../data/demo/python_modelling_framework/deterministic/runoff.py
    ../../data/demo/python_modelling_framework/deterministic/runoffStatic.py
    ../../data/demo/python_modelling_framework/stochastic/montecarlo.py
    ../../data/demo/python_modelling_framework/stochastic/particlefilter.py
    ../../data/demo/python_modelling_framework/stochastic/kalmanfilter.py
)

prepare_pcraster_project_docs("${SPHINX_SOURCES}" "python_modelling_framework" "MODELLING_FRAMEWORK_SPHINX_SOURCES")
