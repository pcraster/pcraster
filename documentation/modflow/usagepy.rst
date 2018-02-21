PCRaster Modflow Python
=======================
This chapter describes the usage of the PCRasterModflow extension within Python scrips. Using the extension requires the specification of the geographic extents and the creation of an extension object:

.. code-block:: python

   from pcraster import *

   setclone("clone.map")
   # Construct Modflow extension object
   mf = initialise(clone())

This will initialise the data structures used in the extension. All operations will operate on the object ``mf``.

The next step is the grid specification using the operations described in the DIS package. This must be done before any other package is defined. Afterwards packages can be defined in arbitrary order. For a Modflow simulation at least the DIS, BAS and BCF package must be specified.

The DIS, BAS, BCF and a solver package must be set in the initial section of a script. Stress packages (RIV, DRN, RCH and WEL) can be activated and modified in the dynamic section.

.. note::

   One timestep in PCRaster represents one stress period in Modflow.

   In the following non-spatial arguments are written capitalised.


.. toctree::
   :maxdepth: 1

   mfin
   mfout