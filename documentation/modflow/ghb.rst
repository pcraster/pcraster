General Head Boundary (GHB) package
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. versionadded:: 4.3

The GHB package allows to set a fixed head boundary. To enable the general head boundary package use:

.. code-block:: python

   mf.setGeneralHead(head, conductance, LAYER)

where

head [:math:`L`]
   is the name of a spatial, scalar PCRaster map containing the head values;

conductance [:math:`L^2T^-1`]
   is the name of a spatial, scalar PCRaster map containing the conductance values. A general head boundary is considered if the conductance value in a cell is larger than zero;

LAYER [:math:`-`]
   is the layer number the map values will be assigned to.

