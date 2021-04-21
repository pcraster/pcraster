Installing PCRaster
===================

Installing binaries
-------------------

PCRaster is available on |condaforgefeedstocksurl| and can be installed using conda.
Supported platforms are Linux, macOS and Windows.

We recommend to download and install |miniconda|.
The user guide and short reference on conda can be found |minicondadoc|.
After install you can check which Python versions are supported by PCRaster:

.. code-block:: console

   conda search -c conda-forge pcraster


Afterwards you can create a new environment and install PCRaster, e.g. with:

.. code-block:: console

   conda create --name pcraster37 python=3.7 -c conda-forge

   conda activate pcraster37

   conda install -c conda-forge pcraster

You can also combine these steps and install additional packages in one go e.g. by:

.. code-block:: console

   conda create --name pcraster37 -c conda-forge python=3.7 pcraster spyder matplotlib





Always use the conda-forge channel when installing further packages into your PCRaster environment.


.. note::
   If you installed previous versions of PCRaster you need to remove their entries from the PATH and PYTHONPATH environment variables before activating your environment.
   Modifying PATH and PYTHONPATH environment variables as required for previous PCRaster versions is not necessary anymore, this is done automatically when you activate your environment.



.. |miniconda| raw:: html

   <a href="https://docs.conda.io/en/latest/miniconda.html" target="_blank">Miniconda</a>

.. |minicondadoc| raw:: html

   <a href="https://docs.conda.io/projects/conda/en/latest/user-guide/cheatsheet.html" target="_blank">here</a>

.. |condaforgefeedstocksurl| raw:: html

   <a href="https://conda-forge.org/feedstocks" target="_blank">conda-forge</a>



Testing your installation
-------------------------

You can execute a few basic steps to check if PCRaster works properly in your conda environment.
In case you have not done yet, activate your PCRaster environment.
Then you can test the visualisation tool Aguila by starting it from the command prompt. It will show its help page.
Afterwards start Python:

.. code-block:: console

   $ conda activate pcraster37
   (pcraster37) $ aguila -h
   (pcraster37) $ python

Import the PCRaster module, print the version number and afterwards create a raster with 6 rows, 4 columns, cell length 1 and 0, 0 as origin.
Fill the entire raster with random values drawn from a uniform distribution and display the result:


.. code-block:: python

   import pcraster as pcr

   pcr.__version__
   pcr.setclone(6, 4, 1, 0, 0)
   pcr.aguila(pcr.uniform(1))

You will see that Aguila is displaying the map:

.. image:: pcraster_python_aguila_conda.png
    :align: center
    :alt: Aguila showing a map with random values, generated with PCRaster Python using conda.


You can also test the PCRaster command line applications.
Exit the Python interpreter and type ``pcrcalc``.
The usage information will be shown:


.. code-block:: console

   (pcraster37) $ pcrcalc
   pcrcalc 4.3.0 (linux/x86_64)
    USAGE: pcrcalc [options] "expression"
    or     pcrcalc [options] -f scriptFile
     ( or #!: pcrcalc -F [options]+)
    other flags:
     s #  : set seed (integer > 0) for random generator
            default is based on current time
     b f  : overrule script bindings
     1    : update timeseries files at end of each timestep
     r f  : set run directory
     d f  : debug mode, check MV creation on assignment
             comparing against clone or areamap boolean mask
     c    : strict Case significant filename check (Unix portability)
     p    : print profile information
     m    : optimize with areamap MV compression
     l    : use less memory but more temporary disk storage
     t    : test argument substitution



Troubleshooting
---------------


Aguila fails to start on macOS with M1 processors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Until now there is no native build of PCRaster for the Apple M1 platform (arm-osx64 on conda).
Users report the successful working of our osx-64 version.
Aguila, however, fails to start up.
This might be resolved by adding the following environment variable:

.. code-block:: console

    export QT_MAC_WANTS_LAYER=1

Import of the ``pcraster`` module fails
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An error message such as

.. code-block:: console

    Traceback (most recent call last):
      File "C:\Software\pcraster\pcraster-4.2.1\python\pcraster\__init__.py", line 21, in <module>
        from ._pcraster import *
    ImportError: DLL load failed while importing _pcraster: Kan opgegeven module niet vinden.

    During handling of the above exception, another exception occurred:

    Traceback (most recent call last):
      File "C:\script.py", line 1, in <module>
        import pcraster as pcr
      File "C:\Software\pcraster\pcraster-4.2.1\python\pcraster\__init__.py", line 55, in <module>
        raise ImportError(msg)
    ImportError: The 'pcraster' module was built for Python 3.6, the version used is 3.8

indicates that an older version of PCRaster is available on the system, here located at ``C:\Software\pcraster\pcraster-4.2.1``.
You need to remove the corresponding PCRaster entries of the PATH and PYTHONPATH environment variables.
After starting a new terminal you can import the ``pcraster`` module.
