PCRaster and LUE
================

You want to run your PCRaster Python model on very large spatial and temporal scales?
Have a look at our `LUE project <https://lue.computationalgeography.org/>`_, a modelling framework for the development of agent-based and field-based models.
It uses the asynchronous many-tasks (AMT) approach for optimal usage of available hardware resources, and resulting LUE models can be executed on laptops or on large computer cluster without changing a single line of the model code.

LUE supports the transition of `PCRaster models to LUE <https://lue.computationalgeography.org/doc/pcraster/port.html>`_, the majority of PCRaster operations already have `LUE equivalents <https://lue.computationalgeography.org/doc/pcraster/status.html#status-compared-with-pcraster>`_.
We ported, for example, the `PyCatch <https://github.com/computationalgeography/pycatch>`_ model and ran it for
`Africa at 3 arc-second resolution <https://lue.computationalgeography.org/blog/2021/04/28/egu2021/>`_, using 12 cluster nodes containing 1152 CPU cores in total.
