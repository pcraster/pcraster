DIS package
^^^^^^^^^^^
The DIS package specifies the grid used for the groundwater model. The grid specification must start with the bottom layer. Afterwards confined/unconfined layer can be added. The maximum number of layers is 999.

The operation for the specification of the bottom layer is

.. code-block:: python

   mf.createBottomLayer(bottomElevation, topElevation)

where

bottomElevation
   is the name of a spatial, scalar PCRaster map containing the bottom elevation values of the layer. The map must not contain missing values;

topElevation
   is the name of a spatial, scalar PCRaster map containing the top of layer elevation values. The map must not contain missing values.

The operation to add a layer is

.. code-block:: python

   mf.addLayer(elevation)

where

elevation
   is the name of a spatial, scalar PCRaster map containing the top of layer elevation values. The map must not contain missing values.

The operation to add a confined layer is

.. code-block:: python

   mf.addConfinedLayer(elevation)

where

elevation
   is the name of a spatial, scalar PCRaster map containing the top of layer elevation values. The map must not contain missing values.

.. figure:: layerdesc.png

   Layer numbering in Modflow and the PCRaster Modflow extension

The figure shows two grid specifications. The left side of the figure represents a four layer system that is specified in a Python script as follows:

.. code-block:: python

   mf.createBottomLayer("bottom.map", "l4top.map")
   mf.addLayer("l3top.map")
   mf.addLayer("l2top.map")
   mf.addLayer("l1top.map")

The right side of the figure shows a three layer system with a confining bed below layer 2:

.. code-block:: python

   mf.createBottomLayer("bottom.map", "l4top.map")
   mf.addConfinedLayer("l3top.map")
   mf.addLayer"(l2top.map")
   mf.addLayer("l1top.map")

The PCRaster Modflow extension uses an opposite layer numbering to the Modflow convention. Furthermore, quasi-3D confining beds obtain a layer number as well. Layer numbering always starts with layer number 1 for the bottom layer and increases for each added confined or unconfined layer.

Except for setting the conductivity values all commands operate on layers which are not specified as confining beds. Attempts to set or retrieve values from confining beds will result in an error.

Optional operations
~~~~~~~~~~~~~~~~~~~
The options for the DIS package can be specified with

.. code-block:: python

   mf.setDISParameter(ITMUNI,LENUNI,PERLEN,NSTP,TSMULT,SSTR)

where

ITMUNI
   indicates the time unit (0: undefined, 1: seconds, 2: minutes, 3: hours, 4: days, 5: years);

LENUNI
   indicates the length unit (0: undefined, 1: feet, 2: meters, 3: centimeters);

PERLEN
   is the duration of a stress period;

NSTP
   is the number of iterations;

TSMULT
   is the multiplier for the length of the successive iterations;

SSTR
   0 - transient, 1 - steady state. If the simulation is set to transient, primary and secondary storage coeffiecents must be set in the BCF package.

All input values are non spatial values. If this operation is not used the simulation will be set to the default values of (undefined, undefined, 1.0, 1, 1.0, 1).

In transient simulations you can use

.. code-block:: python

   mf.updateDISParameter(PERLEN,NSTP,TSMULT)

to change the length of a stress period.

By default, the row and column widths are obtained from the clone map. In case you want to specify widths yourself use

.. code-block:: python

   mf.setRowWidth(values)

and

.. code-block:: python

   mf.setColumnWidth(values)

where values is a Python list containing the widths for each row or column, respectively.
