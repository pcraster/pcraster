

.. index::
   single: spreadmax
.. _spreadmax:

*********
spreadmax
*********
.. topic:: spreadmax

   Total friction of the shortest accumulated friction path over a map with friction values from a source cell to cell under consideration considering maximum spread distance

::

  Result = spreadmax(points, initialfrictiondist, friction, max_distance)

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
   scalar

Operation
=========


Identical to spread but with a fourth parameter, a maximum spread distance. Areas that are not
reached are given the value MV.



Examples
========
