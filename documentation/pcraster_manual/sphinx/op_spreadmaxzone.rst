

.. index::
   single: spreadmaxzone
.. _spreadmaxzone:

*************
spreadmaxzone
*************
.. topic:: spreadmaxzone

   Shortest friction-distance path over a map with friction from an identified source cell or cells to the cell under consideration

::

  Result = spreadmaxzone(points, initialfrictiondist, friction, max_distance)

points
   spatial
   boolean, nominal, ordinal

initialfrictiondist
   spatial, non spatial
   scalar

friction
   spatial, non spatial
   scalar

max_distance
   spatial, non spatial
   scalar

Result
   spatial
   points

Operation
=========


Identical to spreadzone but with a fourth parameter, a maximum spread distance. Areas that are not
reached are given the value 0.



Examples
========
