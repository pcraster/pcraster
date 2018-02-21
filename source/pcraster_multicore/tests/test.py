import string
import math
import sys
import os
import unittest
import test_import
import testexamples_multicore
import testPCRaster
import warnings

import pcraster
from pcraster.multicore import *

import pcraster.multicore._operators as mcop


# Class contains some tests extracted from the Python tests
# that only apply for the multicore module as well
class TestMulticore(unittest.TestCase):

  def testComException(self):
    exceptionThrown = False
    try:
      # Calculating the slope of a boolean map sucks.
      pcraster.setclone("and_Expr1.map")
      res = slope("and_Expr1.map")
    except RuntimeError as exception:
      message = str(exception)
      self.assert_(message.find("argument nr. 1 of function 'slope': type is boolean, legal type is scalar") != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

  def valueTest(self, readValue, readValidValue, trueValidValue, type, trueValue):
    self.assert_(isinstance(readValidValue, int))
    self.assertEqual(readValidValue, trueValidValue)
    if readValidValue:
      self.assert_(isinstance(readValue, type))
      self.assertEqual(readValue, trueValue)

  def testIfThenElse(self):
    pcraster.setclone("and_Expr1.map")
    exceptionThrown = False
    try:
      result = ifthenelse(1.0 == 2.0, 3.0, 4.0)
    except RuntimeError as exception:
      message = str(exception)
      self.assert_(message.find("conversion function to pick a data type") != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

    result = ifthenelse(boolean(1.0 == 2.0), \
         scalar(3.0), scalar(4.0))
    self.assertEqual(pcraster.cellvalue(result, 1)[0], 4.0)

  def testCellValueBoolean(self):
    raster = boolean(pcraster.readmap("and_Expr1.map"))
    value, isValid = pcraster.cellvalue(raster, 1)
    self.valueTest(value, isValid, True, int, True)
    value, isValid = pcraster.cellvalue(raster, 2)
    self.valueTest(value, isValid, True, int, True)
    value, isValid = pcraster.cellvalue(raster, 3)
    self.valueTest(value, isValid, True, int, False)
    value, isValid = pcraster.cellvalue(raster, 4)
    self.valueTest(value, isValid, True, int, False)
    value, isValid = pcraster.cellvalue(raster, 5)
    self.valueTest(value, isValid, False, None, None)
    value, isValid = pcraster.cellvalue(raster, 6)
    self.valueTest(value, isValid, True, int, False)
    value, isValid = pcraster.cellvalue(raster, 7)
    self.valueTest(value, isValid, True, int, True)
    value, isValid = pcraster.cellvalue(raster, 8)
    self.valueTest(value, isValid, True, int, True)
    value, isValid = pcraster.cellvalue(raster, 9)
    self.valueTest(value, isValid, True, int, False)

  def testCellValueNominal(self):
    pcraster.setclone("areaarea_Class.map")
    raster = nominal(pcraster.readmap("areaarea_Class.map"))
    value, isValid = pcraster.cellvalue(raster, 1)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 2)
    value, isValid = pcraster.cellvalue(raster, 2)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 6)
    value, isValid = pcraster.cellvalue(raster, 5)
    self.assertEqual(isValid, False)
    value, isValid = pcraster.cellvalue(raster, 9)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 2)
    value, isValid = pcraster.cellvalue(raster, 25)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 4)

  def testCellValueOrdinal(self):
    pcraster.setclone("areaarea_Class.map")
    raster = ordinal(pcraster.readmap("areaarea_Class.map"))
    value, isValid = pcraster.cellvalue(raster, 1)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 2)
    value, isValid = pcraster.cellvalue(raster, 2)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 6)
    value, isValid = pcraster.cellvalue(raster, 5)
    self.assertEqual(isValid, False)

  def testCellValueScalar(self):
    pcraster.setclone("abs_Expr.map")
    raster = scalar(pcraster.readmap("abs_Expr.map"))
    value, isValid = pcraster.cellvalue(raster, 1)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertEqual(value, 2.0)
    value, isValid = pcraster.cellvalue(raster, 2)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertEqual(value, -7.0)
    value, isValid = pcraster.cellvalue(raster, 3)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertEqual(value, 3.5)
    value, isValid = pcraster.cellvalue(raster, 6)
    self.assertEqual(isValid, False)
    value, isValid = pcraster.cellvalue(raster, 7)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertEqual(value, 0.0)
    value, isValid = pcraster.cellvalue(raster, 8)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertEqual(value, 14.0)

  #def testCellValueDirectional(self):
    #raster = pcraster.readmap("nodirection_Expr.map")
    #value, isValid = pcraster.cellvalue(raster, 1)
    #self.assertEqual(isValid, True)
    #self.assert_(isinstance(value, float))
    #self.assertAlmostEqual(value, math.radians(280))
    #value, isValid = pcraster.cellvalue(raster, 2)
    #self.assertEqual(isValid, True)
    #self.assert_(isinstance(value, float))
    #self.assertAlmostEqual(value, math.radians(25))
    #value, isValid = pcraster.cellvalue(raster, 5)
    #self.assertEqual(isValid, True)
    #self.assert_(isinstance(value, float))
    #self.assertAlmostEqual(value, -1.0)
    #value, isValid = pcraster.cellvalue(raster, 7)
    #self.assertEqual(isValid, False)
    #value, isValid = pcraster.cellvalue(raster, 9)
    #self.assertEqual(isValid, True)
    #self.assert_(isinstance(value, float))
    #self.assertAlmostEqual(value, math.radians(7))

  #def testCellValueLdd(self):
    #pcraster.setclone("accu_Ldd.map")
    #raster = pcraster.readmap("accu_Ldd.map")
    #value, isValid = pcraster.cellvalue(raster, 1)
    #self.assertEqual(isValid, True)
    #self.assert_(isinstance(value, int))
    #self.assertEqual(value, 2)
    #value, isValid = pcraster.cellvalue(raster, 2)
    #self.assertEqual(isValid, True)
    #self.assert_(isinstance(value, int))
    #self.assertEqual(value, 2)
    #value, isValid = pcraster.cellvalue(raster, 9)
    #self.assertEqual(isValid, True)
    #self.assert_(isinstance(value, int))
    #self.assertEqual(value, 1)
    #value, isValid = pcraster.cellvalue(raster, 22)
    #self.assertEqual(isValid, True)
    #self.assert_(isinstance(value, int))
    #self.assertEqual(value, 5)
    #value, isValid = pcraster.cellvalue(raster, 25)
    #self.assertEqual(isValid, True)
    #self.assert_(isinstance(value, int))
    #self.assertEqual(value, 4)

  def testCellValueNonSpatial(self):
    pcraster.setclone("abs_Expr.map")
    raster = pcraster.readmap("abs_Expr.map")
    value, isValid = pcraster.cellvalue(mapmaximum(raster), 1, 1)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertEqual(value, 14.0)
    value, isValid = pcraster.cellvalue(mapmaximum(raster), 1)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertEqual(value, 14.0)

  def testNotEqualsLdd(self):
    pcraster.setclone("accu_Ldd.map")
    ldd = pcraster.readmap("accu_Ldd.map")
    nonSpatial = pcraster.newNonSpatialField(5)
    # we need to explicitly cast PODs to ldd (or directional)
    # when using the multicore module
    #raster = mcop.pcrmcNE("accu_Ldd.map", 5)
    raster = mcop.pcrmcNE("accu_Ldd.map", pcraster.ldd(5))
    warnings.warn("Difference between pcraster and multicore module...")
    value, isValid = pcraster.cellvalue(raster, 1)
    self.assertEqual(isValid, True)
    self.assertEqual(value, True)
    value, isValid = pcraster.cellvalue(raster, 22)
    self.assertEqual(isValid, True)
    self.assertEqual(value, False)
    value, isValid = pcraster.cellvalue(raster, 25)
    self.assertEqual(isValid, True)
    self.assertEqual(value, True)

  def testNominal2Ordinal(self):
    pcraster.setclone("areaarea_Class.map")
    nominalMap = pcraster.readmap("areaarea_Class.map")
    self.assertEqual(nominalMap.dataType(), pcraster.VALUESCALE.Nominal)
    ordinalMap = pcraster.ordinal(nominalMap)
    self.assertEqual(ordinalMap.dataType(), pcraster.VALUESCALE.Ordinal)

  def testOrdinal2Nominal(self):
    ordinalMap = ordinal(pcraster.readmap("areaarea_Class.map"))
    self.assertEqual(ordinalMap.dataType(), pcraster.VALUESCALE.Ordinal)
    nominalMap = nominal(ordinalMap)
    pcraster.report(nominalMap, "nominal.map")
    self.assertEqual(nominalMap.dataType(), pcraster.VALUESCALE.Nominal)

  def testNominal2Scalar(self):
    pcraster.setclone("areaarea_Class.map")
    nominalMap = pcraster.readmap("areaarea_Class.map")
    self.assertEqual(nominalMap.dataType(), pcraster.VALUESCALE.Nominal)
    scalarMap = scalar(nominalMap)
    self.assertEqual(scalarMap.dataType(), pcraster.VALUESCALE.Scalar)

  def testScalar2Nominal(self):
    pcraster.setclone("abs_Expr.map")
    scalarMap = pcraster.readmap("abs_Expr.map")
    self.assertEqual(scalarMap.dataType(), pcraster.VALUESCALE.Scalar)
    nominalMap = nominal(scalarMap)
    self.assertEqual(nominalMap.dataType(), pcraster.VALUESCALE.Nominal)

  def testNonSpatialCreation(self):
    value = 198329008
    nonSpatial = nominal(value)
    self.assertEqual(pcraster.cellvalue(nonSpatial, 1)[0], value)


    # OK
    value = 10000
    i = 0
    while i < 100:
      nonSpatial = nominal(value)
      self.assertEqual(pcraster.cellvalue(nonSpatial, 1)[0], value)
      value += 13
      i += 1

    # Not OK
    bugzilla144 = False
    if not bugzilla144:
      print("skipped bugzilla144")
    else:
      value = 198329008
      i = 0
      while i < 100:
        nonSpatial = nominal(value)
        self.assertEqual(pcraster.cellvalue(nonSpatial, 1)[0], value)
        value += 13
        i += 1

      # Bug is in the _PCRaster/_PCRaster.cc:
      #   newNonSpatialIntegralField code.
      #  CW Jul-14/2008 CW can not trace it back to newNonSpatialIntegralField
      value = 198329012
      nonSpatial = nominal(value)
      self.assertEqual(pcraster.cellvalue(nonSpatial, 1)[0], value)

      value = 198329020
      nonSpatial = nominal(value)
      self.assertEqual(pcraster.cellvalue(nonSpatial, 1)[0], value)

  def testNonSpatialConversions(self):

    pcraster.setclone("map2asc_PCRmap.map")
    nonSpatialValue = mapmaximum(pcraster.readmap("map2asc_PCRmap.map"))

    # Ordinal.
    nonSpatial = ordinal(nonSpatialValue)
    self.assertEqual(bool(nonSpatial), True)
    self.assertEqual(int(nonSpatial), 124)
    self.assertEqual(float(nonSpatial), 124.0)

    # Nominal.
    nonSpatial = nominal(nonSpatialValue)
    self.assertEqual(bool(nonSpatial), True)
    self.assertEqual(int(nonSpatial), 124)
    self.assertEqual(float(nonSpatial), 124)

    # Boolean.
    nonSpatial = boolean(nonSpatialValue)
    self.assertEqual(bool(nonSpatial), True)
    self.assertEqual(int(nonSpatial), 1)
    self.assertEqual(float(nonSpatial), 1.0)

    # Scalar.
    nonSpatial = scalar(mapmaximum("abs_Expr.map"))
    self.assertEqual(bool(nonSpatial), True)
    self.assertEqual(int(nonSpatial), 14)
    self.assertEqual(float(nonSpatial), 14.0)

    # Ldd.
    # TODO  a sensible input
    # nonSpatial = pcraster.ldd(pcraster.mapmaximum(????)))
    # self.assertEqual(bool(nonSpatial), True)
    # self.assertEqual(int(nonSpatial), 6)
    # self.assertEqual(float(nonSpatial), 6.0)

  def testCatchNoneInput(self):
    # all PCRasterPython bindings with an input argument should check on None object

    pcraster.setclone("abs_Expr.map")
    raster = pcraster.readmap("abs_Expr.map")

    exceptionThrown = False
    try:
     raster += None
    except Exception as e:
      #msg = "right operand of operator '+': type is Python None, legal type is scalar"
      msg = "pcraster.multicore add: conversion of argument with type 'NoneType' to PCRaster not possible"
      self.assert_(str(e).find(msg) != -1, str(e))
      exceptionThrown = True
    self.assert_(exceptionThrown)

    self.assertRaises(RuntimeError, pcraster.report, None, "testReportWithNone.map")

  def test_1(self):
    """ test nonspatials nominal in ifthenelse with scalar raster"""
    pcraster.setclone("and_Expr1.map")

    result = ifthenelse(boolean(1.0 == 2.0), \
         scalar(3.0), 4.0)
    self.assertEqual(pcraster.cellvalue(result, 1)[0], 4.0)

    result = ifthenelse(boolean(1.0 == 2.0), \
         scalar(3.0), 4)
    self.assertEqual(pcraster.cellvalue(result, 1)[0], 4.0)


    result = ifthenelse(boolean(2.0 == 2.0), \
         3.0, scalar(4.0))
    self.assertEqual(pcraster.cellvalue(result, 1)[0], 3.0)
    result = ifthenelse(boolean(2.0 == 2.0), \
         3, scalar(4.0))
    self.assertEqual(pcraster.cellvalue(result, 1)[0], 3.0)

  def test_2(self):
    """ test nonspatials nominals in == with scalar raster """
    raster = pcraster.readmap("abs_Expr.map")

    result = raster == -7
    value, isValid = pcraster.cellvalue(result, 1)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 0)
    value, isValid = pcraster.cellvalue(result, 2)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 1)
    value, isValid = pcraster.cellvalue(result, 3)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 0)
    value, isValid = pcraster.cellvalue(result, 4)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 0)
    value, isValid = pcraster.cellvalue(result, 5)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 0)
    value, isValid = pcraster.cellvalue(result, 6)
    self.assertEqual(isValid, False)
    value, isValid = pcraster.cellvalue(result, 7)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 0)
    value, isValid = pcraster.cellvalue(result, 8)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 0)
    value, isValid = pcraster.cellvalue(result, 9)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 0)

  def test_3(self):
    """ test nonspatials nominals in != with scalar raster"""
    raster = pcraster.readmap("abs_Expr.map")

    result = raster != -7
    value, isValid = pcraster.cellvalue(result, 1)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 1)
    value, isValid = pcraster.cellvalue(result, 2)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 0)
    value, isValid = pcraster.cellvalue(result, 3)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 1)
    value, isValid = pcraster.cellvalue(result, 4)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 1)
    value, isValid = pcraster.cellvalue(result, 5)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 1)
    value, isValid = pcraster.cellvalue(result, 6)
    self.assertEqual(isValid, False)
    value, isValid = pcraster.cellvalue(result, 7)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 1)
    value, isValid = pcraster.cellvalue(result, 8)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 1)
    value, isValid = pcraster.cellvalue(result, 9)
    self.assertEqual(isValid, True)
    self.assertEqual(value, 1)

  def test_4(self):
      """ test windowtotal and kernel size larger than raster """
      filename = "windowaverage_Expr.map"
      pcraster.setclone(filename)
      raster = pcraster.readmap(filename)

      result1 = windowtotal(raster, 18)
      result2 = mapmaximum(result1)
      value, isValid = pcraster.cellvalue(result2, 1)
      self.assertEqual(isValid, True)
      self.assertEqual(value, 41)

      pcraster.setglobaloption("unitcell")
      result1 = windowtotal(raster, 9)
      result2 = mapmaximum(result1)
      value, isValid = pcraster.cellvalue(result2, 1)
      self.assertEqual(isValid, True)
      self.assertEqual(value,  41)

  def test_5(self):
      """ test windowaverage and kernel size larger than raster """
      filename = "windowaverage_Expr.map"
      pcraster.setclone(filename)
      raster = pcraster.readmap(filename)

      result1 = windowaverage(raster, 18)
      result2 = mapmaximum(result1)
      value, isValid = pcraster.cellvalue(result2, 1)
      self.assertEqual(isValid, True)
      self.assertAlmostEqual(value, 1.708333, places=6)

      pcraster.setglobaloption("unitcell")
      result1 = windowaverage(raster, 9)
      result2 = mapmaximum(result1)
      value, isValid = pcraster.cellvalue(result2, 1)
      self.assertEqual(isValid, True)
      self.assertAlmostEqual(value,  1.708333, places=6)


suite = unittest.TestSuite()
suite.addTest(unittest.makeSuite(test_import.ImportTest))
suite.addTest(unittest.makeSuite(TestMulticore))
suite.addTest(unittest.makeSuite(testexamples_multicore.TestExamples))
suite.addTest(unittest.makeSuite(testPCRaster.TestPCRaster))

result = unittest.TextTestRunner(verbosity=3).run(suite)
test_result = (0 if result.wasSuccessful() else 1)

sys.exit(test_result)

