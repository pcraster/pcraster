

.. index::
   single: spread
.. _spread:

******
spread
******
.. topic:: spread

   Total friction of the shortest accumulated friction path over a map with friction values from a source cell to cell under consideration

::

  Result = spread(points, initialfrictiondist, friction)

points
   spatial
   boolean, nominal, ordinal

initialfrictiondist
   spatial, non spatial
   scalar

friction
   spatial, non spatial
   scalar

Result
   spatial
   scalar

Options
=======
:literal:`--unittrue` or :literal:`--unitcell`

:literal:`--unittrue`
   distance is measured in true distance (default)

:literal:`--unitcell`
   distance is measured in number of cell lengths



Operation
=========


The expression points identifies those cells from which the shortest accumulated friction path to every cell centre is calculated. The spreading for determination of these friction-distances starts at the centre of cells which have a non zero value on points. The initial friction-distance (at the start of the spreading) is taken from the values at these point cells on initialfrictiondist. During spreading a path is followed over the consecutive neighbouring cells. While following this path the friction-distance increases. The increase of friction-distance per unit distance is specified by the cell values on friction. Using these values, increase when travelling from one cell to its neighbouring cell is calculated as follows: Let friction(sourcecell) and friction(destinationcell) be the friction values at the cell where is moved from and where is moved to, respectively. While moving from the source cell to the destination cell the increase of friction- distance is:



distance x
{(friction(sourcecell)+friction(destinationcell)}/2



where distance is the distance between the sourcecell and the destination
cell. This distance equals the cell length if the source cell and the
destination cell are neighbours in horizontal or vertical directions; it equals
sqrt(2) multiplied by the cell length if the cells are neighbours in
diagonal directions.






During operation of the command, the spreading is executed from all non
zero cells on points, over all possible paths. For determination of the accumulated friction-distance cell values on Result, for each cell the path from a non zero cell on points is chosen with the shortest friction-distance. So during the execution of the spread operation, for each cell, the friction-distance for each possible path from the non zero cells on points to the cell under consideration is calculated and then the path with the shortest friction-distance is chosen. On Result each cell has a value which is the friction-distance covered when moving over this shortest path from a non zero cell on points.

Notes
=====


The values on friction must be larger than zero.



Missing value cells on points, initialfrictiondist and friction are assigned a missing value on Result. Potential shortest paths that cross missing value cells on points, initialfrictiondist or friction are ignored.



If no path is found for a cell (for instance if the cell is surrounded by missing values), a
missing value is assigned to that cell on Result.

Group
=====
This operation belongs to the group of  Neighbourhood operators; spread operators

See Also
========
:ref:`secstatneighfr`, :ref:`ldddist`, :ref:`slopelength`

Examples
========
#.
   | • pcrcalc
   |   binding
   |    Result2 = Result2.map;
   |    Points2 = Points2.map;
   |    Initial2 = Initial2.map;
   |    FrictMat2 = FrictMat2.map;
   |   initial
   |    report Result2 = spread(Points2,Initial2,FrictMat2);
   |
   | • python
   |   Points2 = readmap("Points2.map")
   |   Initial2 = readmap("Initial2.map")
   |   FrictMat2 = readmap("FrictMat2.map")
   |   Result2 = spread(Points2,Initial2,FrictMat2)

   ========================================== ========================================== =========================================== ============================================
   Result2.map                                Points2.map                                Initial2.map                                FrictMat2.map
   .. image::  ../examples/spread_Result2.png .. image::  ../examples/spread_Points2.png .. image::  ../examples/spread_Initial2.png .. image::  ../examples/spread_FrictMat2.png
   ========================================== ========================================== =========================================== ============================================

   |

#.
   | • pcrcalc
   |   binding
   |    Result1 = Result1.map;
   |    Points = Points.map;
   |   initial
   |    report Result1 = spread(Points,0,1);
   |
   | • python
   |   Points = readmap("Points.map")
   |   Result1 = spread(Points,0,1)

   ========================================== =========================================
   Result1.map                                Points.map
   .. image::  ../examples/spread_Result1.png .. image::  ../examples/spread_Points.png
   ========================================== =========================================

   |

