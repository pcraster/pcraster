Installing PCRaster
===================

Installing binaries
-------------------



PCRaster is available on |condaforgeurl| and can be installed using conda.
Supported platforms are Linux, macOS and Windows.


We recommend to download and install |miniconda|.
The user guide and short reference on conda can be found |minicondadoc|.
After install you can check which Python versions are supported by PCRaster:

.. code-block:: bash

   conda search -c conda-forge pcraster


Afterwards you can create a new environment and install PCRaster, e.g. with:


.. code-block:: bash

   conda create --name pcraster37 python=3.7 -c conda-forge

   conda activate pcraster37

   conda install -c conda-forge pcraster

Modifying PATH and PYTHONPATH environment variables as required for previous PCRaster versions is not necessary anymore, this is done automatically when you activate your environment.
If you installed previous versions of PCRaster you need to remove their entries from the PATH and PYTHONPATH environment variables before activating your environment.

Always use the conda-forge channel when installing further packages into your PCRaster environment.


.. |miniconda| raw:: html

   <a href="https://docs.conda.io/en/latest/miniconda.html" target="_blank">Miniconda</a>

.. |minicondadoc| raw:: html

   <a href="https://docs.conda.io/projects/conda/en/latest/user-guide/cheatsheet.html" target="_blank">here</a>

.. |condaforgeurl| raw:: html

   <a href="https://conda-forge.org/" target="_blank">conda-forge</a>




.. include:: build.rst

