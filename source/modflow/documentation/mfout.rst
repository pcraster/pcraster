Retrieving Modflow's output
---------------------------
Afterwards the specification of the grid Modflow can be called with

.. code-block:: python

   mf.run()

After a successful Modflow run the results of a stress period can be reported with the commands described in this section. If the run fails the end of the Modflow list file is displayed, the execution of the script ends.

In Python you can also specify a directory where the Modflow run will be executed, for example in ``1``:

.. code-block:: python

   mf.run("1")

The head and boundary values are retrieved automatically and must not be set again for the next stress period. Applying the operations to layer specified as quasi-3D confining beds will result in an error.

.. code-block:: python

   head = mf.getHeads(LAYER)

``head`` is a scalar PCRaster map containing the resulting head values of the layer ``layer``. Cells converted to dry obtain the data type missing value.

.. code-block:: python

   river = mf.getRiverLeakage(LAYER)

``river`` is a scalar PCRaster map containing the resulting river cell-by-cell flow values (in [:math:`L^3T^-1`]) of the layer ``LAYER``.

.. code-block:: python

   rch = mf.getRecharge(LAYER)

``rch`` is a scalar PCRaster map containing the resulting recharge cell-by-cell flow values (in [:math:`L^3T^-1]`) of the layer ``LAYER``.

.. code-block:: python

   drn = mf.getDrain(LAYER)

``drn`` is a scalar PCRaster map containing the resulting drain cell-by-cell flow values (in [:math:`L^3T^-1]`) of the layer ``LAYER``.


.. code-block:: python

   st = mf.getStorage(LAYER)

``st`` is a scalar PCRaster map containing the resulting cell-by-cell storage terms of the layer ``LAYER``.


.. code-block:: python

   ch = mf.getConstantHead(LAYER)

``ch`` is a scalar PCRaster map containing the resulting cell-by-cell constant head flow terms of the layer ``LAYER``.


.. code-block:: python

   rf = mf.getRightFace(LAYER)

``rf`` is a scalar PCRaster map containing the resulting internal cell-by-cell flows (right) of the layer ``LAYER``.


.. code-block:: python

   ff = mf.getFrontFace(LAYER)

``ff`` is a scalar PCRaster map containing the resulting internal cell-by-cell flows (front) of the layer ``LAYER``.


.. code-block:: python

   lf = mf.getLowerFace(LAYER)

``lf`` is a scalar PCRaster map containing the resulting internal cell-by-cell flows (lower) of the layer ``LAYER``.
