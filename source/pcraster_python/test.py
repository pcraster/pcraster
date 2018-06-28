#!/usr/bin/env pcrPython.sh
# -*- coding: utf-8 -*-

import os, math, string, unittest, warnings, sys, tempfile
import testcase, testexamples, testNumPy, testPickle, testPCRaster
import import_test
import pcraster
import copy

class Test(testcase.TestCase):

  def testComException(self):
    exceptionThrown = False
    try:
      # Calculating the slope of a boolean map sucks.
      slope = pcraster.slope("and_Expr1.map")
    except RuntimeError as exception:
      message = str(exception)
      self.assert_(message.find("argument nr. 1 of function 'slope': type is boolean, legal type is scalar") != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

  def testDalException(self):
    exceptionThrown = False
    try:
      # Trying to read a map that is not there should fail.
      pcraster.readmap("notThere.map")
    except RuntimeError as exception:
      message = str(exception)
      self.assert_(message.find("Raster notThere.map: can not be opened") != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

  def testIfThenElse(self):
    pcraster.setclone("and_Expr1.map")
    exceptionThrown = False
    try:
      result = pcraster.ifthenelse(1.0 == 2.0, 3.0, 4.0)
    except RuntimeError as exception:
      message = str(exception)
      self.assert_(message.find("conversion function to pick a data type") != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

    result = pcraster.ifthenelse(pcraster.boolean(1.0 == 2.0), \
         pcraster.scalar(3.0), pcraster.scalar(4.0))
    self.assertEqual(pcraster.cellvalue(result, 1)[0], 4.0)

  def valueTest(self, readValue, readValidValue, trueValidValue, type, trueValue):
    self.assert_(isinstance(readValidValue, int))
    self.assertEqual(readValidValue, trueValidValue)
    if readValidValue:
      self.assert_(isinstance(readValue, type))
      self.assertEqual(readValue, trueValue)

  def testCellValueBoolean(self):
    raster = pcraster.readmap("and_Expr1.map")
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
    raster = pcraster.readmap("areaarea_Class.map")
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
    raster = pcraster.ordinal(pcraster.readmap("areaarea_Class.map"))
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
    raster = pcraster.readmap("abs_Expr.map")
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

  def testCellValueDirectional(self):
    raster = pcraster.readmap("nodirection_Expr.map")
    value, isValid = pcraster.cellvalue(raster, 1)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertAlmostEqual(value, math.radians(280))
    value, isValid = pcraster.cellvalue(raster, 2)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertAlmostEqual(value, math.radians(25))
    value, isValid = pcraster.cellvalue(raster, 5)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertAlmostEqual(value, -1.0)
    value, isValid = pcraster.cellvalue(raster, 7)
    self.assertEqual(isValid, False)
    value, isValid = pcraster.cellvalue(raster, 9)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertAlmostEqual(value, math.radians(7))

  def testCellValueLdd(self):
    raster = pcraster.readmap("accu_Ldd.map")
    value, isValid = pcraster.cellvalue(raster, 1)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 2)
    value, isValid = pcraster.cellvalue(raster, 2)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 2)
    value, isValid = pcraster.cellvalue(raster, 9)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 1)
    value, isValid = pcraster.cellvalue(raster, 22)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 5)
    value, isValid = pcraster.cellvalue(raster, 25)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, int))
    self.assertEqual(value, 4)

  def testCellValueNonSpatial(self):
    raster = pcraster.readmap("abs_Expr.map")
    value, isValid = pcraster.cellvalue(pcraster.mapmaximum(raster), 1, 1)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertEqual(value, 14.0)
    value, isValid = pcraster.cellvalue(pcraster.mapmaximum(raster), 1)
    self.assertEqual(isValid, True)
    self.assert_(isinstance(value, float))
    self.assertEqual(value, 14.0)

  def testNotEqualsLdd(self):
    ldd = pcraster.readmap("accu_Ldd.map")
    nonSpatial = pcraster.newNonSpatialField(5)
    raster = pcraster.pcrne("accu_Ldd.map", 5)
    value, isValid = pcraster.cellvalue(raster, 1)
    self.assertEqual(isValid, True)
    self.assertEqual(value, True)
    value, isValid = pcraster.cellvalue(raster, 22)
    self.assertEqual(isValid, True)
    self.assertEqual(value, False)
    value, isValid = pcraster.cellvalue(raster, 25)
    self.assertEqual(isValid, True)
    self.assertEqual(value, True)

  def testReportFirstArgIsFilename(self):
    inputFilename = "and_Expr1.map"
    outputFilename = tempfile.NamedTemporaryFile().name
    pcraster.report(inputFilename, outputFilename)
    self.failUnless(self.mapEqualsValidated(pcraster.readmap(inputFilename), outputFilename))
    os.remove(outputFilename)

  def testReportNonSpatial(self):
    raster = pcraster.readmap("abs_Expr.map")
    max1 = pcraster.mapmaximum(raster)
    value, isValid = pcraster.cellvalue(max1, 1)
    self.assert_(isinstance(value, float))
    self.assertEqual(isValid, True)
    self.assertEqual(value, 14.0)
    pcraster.report(max1, "maximum.map")
    max2 = pcraster.readmap("maximum.map")

    for i in range(1, 8):
      value, isValid = pcraster.cellvalue(max2, i)
      self.assertEqual(isValid, True)
      self.assert_(isinstance(value, float))
      self.assertEqual(value, 14.0)

  def testNominal2Ordinal(self):
    nominalMap = pcraster.readmap("areaarea_Class.map")
    self.assertEqual(nominalMap.dataType(), pcraster.VALUESCALE.Nominal)
    ordinalMap = pcraster.ordinal(nominalMap)
    self.assertEqual(ordinalMap.dataType(), pcraster.VALUESCALE.Ordinal)

  def testOrdinal2Nominal(self):
    ordinalMap = pcraster.ordinal(pcraster.readmap("areaarea_Class.map"))
    self.assertEqual(ordinalMap.dataType(), pcraster.VALUESCALE.Ordinal)
    nominalMap = pcraster.nominal(ordinalMap)
    pcraster.report(nominalMap, "nominal.map")
    self.assertEqual(nominalMap.dataType(), pcraster.VALUESCALE.Nominal)

  def testNominal2Scalar(self):
    nominalMap = pcraster.readmap("areaarea_Class.map")
    self.assertEqual(nominalMap.dataType(), pcraster.VALUESCALE.Nominal)
    scalarMap = pcraster.scalar(nominalMap)
    self.assertEqual(scalarMap.dataType(), pcraster.VALUESCALE.Scalar)

  def testScalar2Nominal(self):
    scalarMap = pcraster.readmap("abs_Expr.map")
    self.assertEqual(scalarMap.dataType(), pcraster.VALUESCALE.Scalar)
    nominalMap = pcraster.nominal(scalarMap)
    self.assertEqual(nominalMap.dataType(), pcraster.VALUESCALE.Nominal)

  def testNonSpatialCreation(self):
    value = 198329008
    nonSpatial = pcraster.nominal(value)
    self.assertEqual(pcraster.cellvalue(nonSpatial, 1)[0], value)


    # OK
    value = 10000
    i = 0
    while i < 100:
      nonSpatial = pcraster.nominal(value)
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
        nonSpatial = pcraster.nominal(value)
        self.assertEqual(pcraster.cellvalue(nonSpatial, 1)[0], value)
        value += 13
        i += 1

      # Bug is in the _PCRaster/_PCRaster.cc:
      #   newNonSpatialIntegralField code.
      #  CW Jul-14/2008 CW can not trace it back to newNonSpatialIntegralField
      value = 198329012
      nonSpatial = pcraster.nominal(value)
      self.assertEqual(pcraster.cellvalue(nonSpatial, 1)[0], value)

      value = 198329020
      nonSpatial = pcraster.nominal(value)
      self.assertEqual(pcraster.cellvalue(nonSpatial, 1)[0], value)

  def testNonZero(self):
    raster1 = pcraster.readmap("abs_Expr.map")
    raster2 = pcraster.readmap("abs_Expr.map")

    exceptionThrown = False
    ambiguousSpatialMsg = "The truth value for PCRaster spatial data types is ambiguous. "
    try:
      bool(raster1)
    except Exception as exception:
      message = str(exception)
      self.assert_(message.find(ambiguousSpatialMsg) != -1)
    exceptionThrown = True
    self.assert_(exceptionThrown)

    exceptionThrown = False
    try:
      if raster1 == raster2:
        pass
    except Exception as exception:
      message = str(exception)
      self.assert_(message.find(ambiguousSpatialMsg) != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

    exceptionThrown = False
    try:
      while raster1:
        print("this will run forever")
    except Exception as exception:
      message = str(exception)
      self.assert_(message.find(ambiguousSpatialMsg) != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

    exceptionThrown = False
    try:
      if raster1:
        pass
    except Exception as exception:
      message = str(exception)
      self.assert_(message.find(ambiguousSpatialMsg) != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

    exceptionThrown = False
    try:
      tmp = raster1 and raster2
    except Exception as exception:
      message = str(exception)
      self.assert_(message.find(ambiguousSpatialMsg) != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

    exceptionThrown = False
    try:
      tmp = raster1 or raster2
    except Exception as exception:
      message = str(exception)
      self.assert_(message.find(ambiguousSpatialMsg) != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

    exceptionThrown = False
    try:
      tmp = not raster1
    except Exception as exception:
      message = str(exception)
      self.assert_(message.find(ambiguousSpatialMsg) != -1)
      exceptionThrown = True
    self.assert_(exceptionThrown)

  def testReadMap(self):
    w = pcraster.readmap("nominalUINT1.map")
    self.assert_(w.isSpatial())

  def testReadFieldCell(self):
    value,mv = pcraster.readFieldCell("abs_Expr.map", 1, 1)
    self.assertEqual(value,  2.0)
    self.assertEqual(mv, True)
    value,mv = pcraster.readFieldCell("abs_Expr.map", 1, 2)
    self.assertEqual(value,  -7.0)
    self.assertEqual(mv, True)
    value,mv = pcraster.readFieldCell("abs_Expr.map", 1, 3)
    self.assertEqual(value,  3.5)
    self.assertEqual(mv, True)
    value,mv = pcraster.readFieldCell("abs_Expr.map", 2, 1)
    self.assertEqual(value,  -8.5)
    self.assertEqual(mv, True)
    value,mv = pcraster.readFieldCell("abs_Expr.map", 2, 2)
    self.assertAlmostEqual(value,  3.6, 6)
    self.assertEqual(mv, True)
    value,mv = pcraster.readFieldCell("abs_Expr.map", 2, 3)
    self.assertEqual(mv, False)
    value,mv = pcraster.readFieldCell("abs_Expr.map", 3, 1)
    self.assertEqual(value,  0.0)
    self.assertEqual(mv, True)
    value,mv = pcraster.readFieldCell("abs_Expr.map", 3, 2)
    self.assertEqual(value,  14.0)
    self.assertEqual(mv, True)
    value,mv = pcraster.readFieldCell("abs_Expr.map", 3, 3)
    self.assertAlmostEqual(value,  -0.8)
    self.assertEqual(mv, True)

  def testNonSpatialConversions(self):
    nonSpatialValue = pcraster.mapmaximum(pcraster.readmap("map2asc_PCRmap.map"))
    # Ordinal.
    nonSpatial = pcraster.ordinal(nonSpatialValue)
    self.assertEqual(bool(nonSpatial), True)
    self.assertEqual(int(nonSpatial), 124)
    self.assertEqual(float(nonSpatial), 124.0)

    # Nominal.
    nonSpatial = pcraster.nominal(nonSpatialValue)
    self.assertEqual(bool(nonSpatial), True)
    self.assertEqual(int(nonSpatial), 124)
    self.assertEqual(float(nonSpatial), 124)

    # Boolean.
    nonSpatial = pcraster.boolean(nonSpatialValue)
    self.assertEqual(bool(nonSpatial), True)
    self.assertEqual(int(nonSpatial), 1)
    self.assertEqual(float(nonSpatial), 1.0)

    # Scalar.
    nonSpatial = pcraster.scalar(pcraster.mapmaximum("abs_Expr.map"))
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

    raster = pcraster.readmap("abs_Expr.map")

    exceptionThrown = False
    try:
     raster += None
    except Exception as e:
      msg = "right operand of operator '+': type is Python None, legal type is scalar"
      self.assert_(str(e).find(msg) != -1, str(e))
      exceptionThrown = True
    self.assert_(exceptionThrown)

    self.assertRaises(RuntimeError, pcraster.report, None, "testReportWithNone.map")

  def testDeepCopyRaster(self):
    raster = pcraster.readmap(os.path.join("validated", "boolean_Result.map"))
    tmp = copy.deepcopy(raster)
    outputFilename = tempfile.NamedTemporaryFile().name
    pcraster.report(tmp, outputFilename)
    self.failUnless(self.mapEqualsValidated(tmp, "boolean_Result.map"))
    os.remove(outputFilename)

    raster = pcraster.readmap(os.path.join("validated", "nominal_Result.map"))
    tmp = copy.deepcopy(raster)
    outputFilename = tempfile.NamedTemporaryFile().name
    pcraster.report(tmp, outputFilename)
    self.failUnless(self.mapEqualsValidated(tmp, "nominal_Result.map"))
    os.remove(outputFilename)

    raster = pcraster.readmap(os.path.join("validated", "ordinal_Result.map"))
    tmp = copy.deepcopy(raster)
    outputFilename = tempfile.NamedTemporaryFile().name
    pcraster.report(tmp, outputFilename)
    self.failUnless(self.mapEqualsValidated(tmp, "ordinal_Result.map"))
    os.remove(outputFilename)

    raster = pcraster.readmap(os.path.join("validated", "scalar_Result.map"))
    tmp = copy.deepcopy(raster)
    outputFilename = tempfile.NamedTemporaryFile().name
    pcraster.report(tmp, outputFilename)
    self.failUnless(self.mapEqualsValidated(tmp, "scalar_Result.map"))
    os.remove(outputFilename)

    raster = pcraster.readmap(os.path.join("validated", "directional_Result1.map"))
    tmp = copy.deepcopy(raster)
    outputFilename = tempfile.NamedTemporaryFile().name
    pcraster.report(tmp, outputFilename)
    self.failUnless(self.mapEqualsValidated(tmp, "directional_Result1.map"))
    os.remove(outputFilename)

    raster = pcraster.readmap(os.path.join("validated", "ldd_Result.map"))
    tmp = copy.deepcopy(raster)
    outputFilename = tempfile.NamedTemporaryFile().name
    pcraster.report(tmp, outputFilename)
    self.failUnless(self.mapEqualsValidated(tmp, "ldd_Result.map"))
    os.remove(outputFilename)


  def testDeepCopyRasterNonSpatial(self):
    pcraster.setclone("validated/boolean_Result.map")

    raster = pcraster.boolean(1)
    tmp = copy.deepcopy(raster)
    self.assertEqual(True, self.arbitraryMapEquals(raster, tmp))
    raster1 = pcraster.nominal(1)
    tmp1 = copy.deepcopy(raster1)
    self.assertEqual(True, self.arbitraryMapEquals(raster1, tmp1))
    raster2 = pcraster.ordinal(1)
    tmp2 = copy.deepcopy(raster2)
    self.assertEqual(True, self.arbitraryMapEquals(raster2, tmp2))
    raster3 = pcraster.scalar(1)
    tmp3 = copy.deepcopy(raster3)
    self.assertEqual(True, self.arbitraryMapEquals(raster3, tmp3))
    raster4 = pcraster.directional(1)
    tmp4 = copy.deepcopy(raster4)
    self.assertEqual(True, self.arbitraryMapEquals(raster4, tmp4))
    raster5 = pcraster.ldd(1)
    tmp5 = copy.deepcopy(raster5)
    self.assertEqual(True, self.arbitraryMapEquals(raster5, tmp5))


  def testCopyRaster(self):
    raster = pcraster.readmap(os.path.join("validated","boolean_Result.map"))
    exceptionThrown = False
    try:
      tmp = copy.copy(raster)
    except Exception as e:
      self.assertEqual(str(e), "Shallow copy of PCRaster objects not supported\n")
      exceptionThrown = True
    self.assert_(exceptionThrown)


    raster = pcraster.readmap(os.path.join("validated","nominal_Result.map"))
    exceptionThrown = False
    try:
      tmp = copy.copy(raster)
    except Exception as e:
      self.assertEqual(str(e), "Shallow copy of PCRaster objects not supported\n")
      exceptionThrown = True
    self.assert_(exceptionThrown)

    raster = pcraster.readmap(os.path.join("validated","ordinal_Result.map"))
    exceptionThrown = False
    try:
      tmp = copy.copy(raster)
    except Exception as e:
      self.assertEqual(str(e), "Shallow copy of PCRaster objects not supported\n")
      exceptionThrown = True
    self.assert_(exceptionThrown)

    raster = pcraster.readmap(os.path.join("validated","scalar_Result.map"))
    exceptionThrown = False
    try:
      tmp = copy.copy(raster)
    except Exception as e:
      self.assertEqual(str(e), "Shallow copy of PCRaster objects not supported\n")
      exceptionThrown = True
    self.assert_(exceptionThrown)

    raster = pcraster.readmap(os.path.join("validated","directional_Result1.map"))
    exceptionThrown = False
    try:
      tmp = copy.copy(raster)
    except Exception as e:
      self.assertEqual(str(e), "Shallow copy of PCRaster objects not supported\n")
      exceptionThrown = True
    self.assert_(exceptionThrown)

    raster = pcraster.readmap(os.path.join("validated","ldd_Result.map"))
    exceptionThrown = False
    try:
      tmp = copy.copy(raster)
    except Exception as e:
      self.assertEqual(str(e), "Shallow copy of PCRaster objects not supported\n")
      exceptionThrown = True
    self.assert_(exceptionThrown)

  def test1(self):
    """ test clone/areamap not set """
    exceptionThrown = False
    try:
      result = pcraster.uniform(1)
    except Exception as e:
      self.assertEqual(str(e), "uniform: no clone or area map specified, use setclone()\n")
      exceptionThrown = True
    self.assert_(exceptionThrown)

  def testSetCloneUsingFile(self):
    pcraster.setclone(os.path.join("validated", "ordinal_Result.map"))
    self.assertEqual(pcraster.clone().nrRows(), 3)
    self.assertEqual(pcraster.clone().nrCols(), 3)
    self.assertEqual(pcraster.clone().cellSize(), 1.0)
    self.assertEqual(pcraster.clone().west(), 0.0)
    self.assertEqual(pcraster.clone().north(), 0.0)

  def testSetCloneUsingValues(self):
    nrRows = 4
    nrCols = 5
    cellSize = 6.0
    west = 7.0
    north = 8.0
    pcraster.setclone(nrRows, nrCols, cellSize, west, north)
    self.assertEqual(pcraster.clone().nrRows(), nrRows)
    self.assertEqual(pcraster.clone().nrCols(), nrCols)
    self.assertEqual(pcraster.clone().cellSize(), cellSize)
    self.assertEqual(pcraster.clone().west(), west)
    self.assertEqual(pcraster.clone().north(), north)

  def testArgOrder(self):
    chances1 = pcraster.readmap("argorderwithidarealimited_Chances11.map")
    chances2 = pcraster.readmap("argorderwithidarealimited_Chances12.map")
    result = pcraster.argorder(chances1, chances2)
    self.failUnless(self.mapEqualsValidated(result, "argorder_Result.map"))


suite = unittest.TestSuite()
suite.addTest(unittest.makeSuite(Test))
suite.addTest(unittest.makeSuite(testexamples.TestExamples))
suite.addTest(unittest.makeSuite(testNumPy.TestNumPy))
suite.addTest(unittest.makeSuite(import_test.ImportTest))
suite.addTest(unittest.makeSuite(testPickle.TestPickle))
suite.addTest(unittest.makeSuite(testPCRaster.TestPCRaster))

result = unittest.TextTestRunner(verbosity=3).run(suite)
test_result = (0 if result.wasSuccessful() else 1)

sys.exit(test_result)
