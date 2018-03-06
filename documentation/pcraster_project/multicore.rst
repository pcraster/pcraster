The PCRaster Python multicore module
====================================

.. versionadded:: 4.2

Introduction
------------

In case your installation of PCRaster contains the pcraster.multicore
submodule (default on Windows), then you can use it to speed up certain operations. This
module contains alternative implementations of a subset of the PCRaster
operations that are capable of distributing their workload across more
than one CPU core. Check module description of the pcraster.multicore module for a
list of supported operations (mostly local and focal operations).


Usage
-----

To use these operations, all you have to do is set the environment variable
`PCRASTER_NR_WORKER_THREADS` to the number of threads PCRaster should
use. In case your machine has not much else to do you can set it to the
number of CPU cores available. Otherwise, choose a smaller value.

To use, for example, 4 worker threads on Windows use ::

  set PCRASTER_NR_WORKER_THREADS=4

and on Unices use ::

  export PCRASTER_NR_WORKER_THREADS=4

Changes to your model script are not required, the multicore module will check whether the provided arguments of
operations are supported by the multicore module.
If not, the module will fall back to the classic PCRaster implementation.

In case of focal operations, like windowaverage and windowtotal,
you must provide a window length that corresponds to an odd number of
cells. The underlying multicore implementation is faster than the classic PCRaster
implementation, but does not support handling fractions of cells, which
the original algorithm does. So, when the window size is not an odd
integral value, the classic implementation will be used.


Reference documentation
-----------------------

Module description
~~~~~~~~~~~~~~~~~~

.. automodule:: pcraster.multicore
   :members:

List of operations
~~~~~~~~~~~~~~~~~~

.. automodule:: pcraster.multicore._operations
   :members:


Indices and tables
------------------

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
