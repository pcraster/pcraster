import math
import unittest
import numpy
import pcraster
import testcase


class TestNumPy(testcase.TestCase):
  def testBooleanRaster2Array(self):
    raster = pcraster.readmap("and_Expr1.map")
    mv = 99
    array = pcraster.pcr2numpy(raster, mv)
    self.assert_(isinstance(array[0][0], numpy.uint8))
    self.assertEqual(array[0][0], 1)
    self.assertEqual(array[0][1], 1)
    self.assertEqual(array[0][2], 0)
    self.assertEqual(array[1][0], 0)
    self.assertEqual(array[1][1], mv)
    self.assertEqual(array[1][2], 0)
    self.assertEqual(array[2][0], 1)
    self.assertEqual(array[2][1], 1)
    self.assertEqual(array[2][2], 0)


  def testNominalRaster2Array(self):
    raster = pcraster.readmap("areaarea_Class.map")
    mv = 99
    array = pcraster.pcr2numpy(raster, mv)
    self.assert_(isinstance(array[0][0], numpy.int32))
    self.assertEqual(array[0][0], 2)
    self.assertEqual(array[0][1], 6)
    self.assertEqual(array[0][2], 2)
    self.assertEqual(array[0][3], 2)
    self.assertEqual(array[0][4], mv)
    self.assertEqual(array[1][0], 6)
    self.assertEqual(array[1][1], 6)
    self.assertEqual(array[1][2], 2)
    self.assertEqual(array[1][3], 2)
    self.assertEqual(array[1][4], 2)
    self.assertEqual(array[2][0], 6)
    self.assertEqual(array[2][1], 6)
    self.assertEqual(array[2][2], 0)
    self.assertEqual(array[2][3], 0)
    self.assertEqual(array[2][4], 0)
    self.assertEqual(array[3][0], 6)
    self.assertEqual(array[3][1], 6)
    self.assertEqual(array[3][2], 0)
    self.assertEqual(array[3][3], 0)
    self.assertEqual(array[3][4], 0)
    self.assertEqual(array[4][0], 6)
    self.assertEqual(array[4][1], 3)
    self.assertEqual(array[4][2], 3)
    self.assertEqual(array[4][3], 4)
    self.assertEqual(array[4][4], 4)


  def testOrdinalRaster2Array(self):
    raster = pcraster.readmap("succ_Expr.map")
    mv = 99
    array = pcraster.pcr2numpy(raster, mv)
    self.assert_(isinstance(array[0][0], numpy.int32))
    self.assertEqual(array[0][0],-5)
    self.assertEqual(array[0][1], 9)
    self.assertEqual(array[0][2], 9)
    self.assertEqual(array[0][3], 0)
    self.assertEqual(array[1][0],-5)
    self.assertEqual(array[1][1],-5)
    self.assertEqual(array[1][2], 9)
    self.assertEqual(array[1][3], 0)
    self.assertEqual(array[2][0],-5)
    self.assertEqual(array[2][1], 9)
    self.assertEqual(array[2][2], 9)
    self.assertEqual(array[2][3], 2)
    self.assertEqual(array[3][0], 4)
    self.assertEqual(array[3][1], 4)
    self.assertEqual(array[3][2], 9)
    self.assertEqual(array[3][3],mv)


  def testScalarRaster2Array(self):
    raster = pcraster.readmap("abs_Expr.map")
    mv = 99
    array = pcraster.pcr2numpy(raster, mv)
    self.assert_(isinstance(array[0][0], numpy.float32))
    self.assertEqual(array[0][0],  2.0)
    self.assertEqual(array[0][1], -7.0)
    self.assertEqual(array[0][2],  3.5)
    self.assertEqual(array[1][0], -8.5)
    self.assertAlmostEqual(array[1][1], 3.6, 6)
    self.assertEqual(array[1][2], mv)
    self.assertEqual(array[2][0],  0.0)
    self.assertEqual(array[2][1], 14.0)
    self.assertAlmostEqual(array[2][2], -0.8)


  def testLddRaster2Array(self):
    raster = pcraster.readmap("accu_Ldd.map")
    mv = 99
    array = pcraster.pcr2numpy(raster, mv)
    self.assert_(isinstance(array[0][0], numpy.uint8))
    self.assertEqual(array[0][0], 2)
    self.assertEqual(array[0][1], 2)
    self.assertEqual(array[0][2], 2)
    self.assertEqual(array[0][3], 1)
    self.assertEqual(array[0][4], 1)
    self.assertEqual(array[1][0], 2)
    self.assertEqual(array[1][1], 2)
    self.assertEqual(array[1][2], 1)
    self.assertEqual(array[1][3], 1)
    self.assertEqual(array[1][4], 1)
    self.assertEqual(array[2][0], 3)
    self.assertEqual(array[2][1], 2)
    self.assertEqual(array[2][2], 1)
    self.assertEqual(array[2][3], 4)
    self.assertEqual(array[2][4], 1)
    self.assertEqual(array[3][0], 3)
    self.assertEqual(array[3][1], 2)
    self.assertEqual(array[3][2], 1)
    self.assertEqual(array[3][3], 4)
    self.assertEqual(array[3][4], 4)
    self.assertEqual(array[4][0], 6)
    self.assertEqual(array[4][1], 5)
    self.assertEqual(array[4][2], 4)
    self.assertEqual(array[4][3], 4)
    self.assertEqual(array[4][4], 4)


  def testBooleanArray2Raster(self):
    pcraster.setclone("boolean_Expr.map")
    try:
      a = numpy.array([ [1, 0, 1], [20, 1, 1], [1, 1, 0] ], numpy.uint8) # uint8 is bugzilla #271
      result = pcraster.numpy2pcr(pcraster.Boolean, a, 20)
      self.failUnless(self.mapEqualsValidated(result, "boolean_Result.map"), "test1: %s" % ("Result and validated result are not the same"))
    except Exception as exception:
      self.failUnless(False, "test1: %s" % (str(exception)))

  def testBooleanArray2RasterDomainError(self):
    pcraster.setclone("boolean_Expr.map")
    a = numpy.array([ [1, 0, 1], [20, 100, 1], [1, 1, 0] ]) # 100 is domain error
    self.assertRaises(Exception,pcraster.numpy2pcr,pcraster.Boolean, a, 20)

  def testNominalArray2Raster(self):
    pcraster.setclone("boolean_Expr.map")
    try:
      a = numpy.array([ [0, 1, 3], [20, -3, -2], [0, 9, 8] ])
      result = pcraster.numpy2pcr(pcraster.Nominal, a, 20)
      self.failUnless(self.mapEqualsValidated(result, "nominal_Result.map"), "test1: %s" % ("Result and validated result are not the same"))
    except Exception as exception:
      self.failUnless(False, "test1: %s" % (str(exception)))


  def testOrdinalArray2Raster(self):
    pcraster.setclone("boolean_Expr.map")
    try:
      a = numpy.array([ [0, 1, 3], [20, -3, -2], [0, 9, 8] ])
      result = pcraster.numpy2pcr(pcraster.Ordinal, a, 20)
      self.failUnless(self.mapEqualsValidated(result, "ordinal_Result.map"), "test1: %s" % ("Result and validated result are not the same"))
    except Exception as exception:
      self.failUnless(False, "test1: %s" % (str(exception)))

  def testScalarArray2Raster(self):
    pcraster.setclone("boolean_Expr.map")
    try:
      a = numpy.array([ [0.5, 0.34202, 0.310676], [20, 0, -0.981627], [0.707107, 0.144356, 0.0174524] ])
      result = pcraster.numpy2pcr(pcraster.Scalar, a, 20)
      self.failUnless(self.mapEqualsValidated(result, "sin_Result.map"), "test1: %s" % ("Result and validated result are not the same"))
    except Exception as exception:
      self.failUnless(False, "test1: %s" % (str(exception)))


  def testDirectionalArray2Raster(self):
    pcraster.setclone("boolean_Expr.map")
    pcraster.setglobaloption("degrees")
    try:
      a = numpy.array([ [math.radians(350),math.radians(0),math.radians(0.01)],\
         [20,math.radians(350),math.radians(21)],\
         [math.radians(359),math.radians(40),math.radians(0)] ])
      result = pcraster.numpy2pcr(pcraster.Directional, a, 20)
      self.failUnless(self.mapEqualsValidated(result, "directional_Result2.map"), "test1: %s" % ("Result and validated result are not the same"))
    except Exception as exception:
      self.failUnless(False, "test1: %s" % (str(exception)))


  def testLddArray2Raster(self):
    pcraster.setclone("and_Expr1.map")
    try:
      a = numpy.array([ [6, 5, 4], [6, 8, 7], [8, 8, 8] ])
      result = pcraster.numpy2pcr(pcraster.Ldd, a, 20)
      self.failUnless(self.mapEqualsValidated(result, "ldd_Result.map"), "test1: %s" % ("Result and validated result are not the same"))
    except Exception as exception:
      self.failUnless(False, "test1: %s" % (str(exception)))

  def test_round_trip_numpy_array_with_nan(self):
      array = numpy.array([
          [-2, -1],
          [ 0, numpy.nan],
          [ 1, 2 ]
      ])
      nrRows, nrCols, cellSize = 3, 2, 1.0
      west, north = 0.0, 0.0
      pcraster.setclone(nrRows, nrCols, cellSize, west, north)
      raster = pcraster.numpy2pcr(pcraster.Scalar, array, numpy.nan)
      array2 = pcraster.pcr2numpy(raster, numpy.nan)
      self.assertEqual(array2[0][0], -2)
      self.assertEqual(array2[0][1], -1)
      self.assertEqual(array2[1][0], 0)
      self.assertTrue(numpy.isnan(array2[1][1]))
      self.assertEqual(array2[2][0], 1)
      self.assertEqual(array2[2][1], 2)

  def test_pcr_as_numpy(self):
      array = numpy.array([
          [-2.0, -1.0      ],
          [ 0.0,  numpy.nan],
          [ 1.0,  2.0      ]
      ])
      nrRows, nrCols, cellSize = 3, 2, 1.0
      west, north = 0.0, 0.0
      pcraster.setclone(nrRows, nrCols, cellSize, west, north)

      # Create a raster.
      raster = pcraster.numpy2pcr(pcraster.Scalar, array, 999.0)

      # Test type checking.
      with self.assertRaises(Exception) as context_manager:
          pcraster.pcr_as_numpy(5)
      self.assertEqual(str(context_manager.exception),
          "Expecting a PCRaster field")

      # Create an array referencing the raster.
      array2 = pcraster.pcr_as_numpy(raster)
      self.assertEqual(array2[0][0], -2)
      self.assertEqual(array2[0][1], -1)
      self.assertEqual(array2[1][0], 0)
      self.assertTrue(numpy.isnan(array2[1][1]))
      self.assertEqual(array2[2][0], 1)
      self.assertEqual(array2[2][1], 2)

      # Change the array and verify the raster changed too.
      array2[0][0] = 5.0
      self.assertEqual(pcraster.pcr2numpy(raster, 999.0)[0][0], 5.0)

      # Replace exising raster and verify the array still behaves.
      raster += 1.0
      self.assertEqual(array2[0][0], 5.0)

      # Delete the raster and verify the array still behaves.
      del raster
      self.assertEqual(array2[0][0], 5.0)
      self.assertEqual(array2[2][1], 2.0)


  @unittest.skip("see gh140")
  def test_numpy2pcr(self):
      nrRows, nrCols, cellSize = 3, 2, 1.0
      west, north = 0.0, 0.0
      pcraster.setclone(nrRows, nrCols, cellSize, west, north)

      # Values in array must fit the value scale of the raster exactly. This
      # will be checked.

      # Valid boolean values are: 0, 1, missing_value.
      # Valid ldd values are: 1, 2, 3, 4, 5, 6, 7, 8, 9, missing_value.
      # Valid nominal values are: [-2^31 + 1, 2^31], missing_value.
      # Valid ordinal values are: [-2^31 + 1, 2^31], missing_value.
      # Valid scalar values are: All 32 bit float values.
      # Valid directional values are: All 32 bit float values.

      # bool_min = 0
      # bool_max = 1
      int8_min = numpy.iinfo(numpy.int8).min
      int8_max = numpy.iinfo(numpy.int8).max
      int32_min = numpy.iinfo(numpy.int32).min
      int32_max = numpy.iinfo(numpy.int32).max
      int64_min = numpy.iinfo(numpy.int64).min
      int64_max = numpy.iinfo(numpy.int64).max

      # bool -> Boolean (uint8)
      raster = pcraster.numpy2pcr(pcraster.Boolean, numpy.array([
          [1,  1],
          [0,  5],
          [1,  1]], numpy.bool), 5)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (True, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), (False, True))
      # It is not possible to create a bool array with other values than
      # 0 and 1. Passing 5 as missing value has no effect.
      self.assertEqual(pcraster.cellvalue(raster, 2, 2), (True, True))

      # int8 -> Boolean (uint8)
      raster = pcraster.numpy2pcr(pcraster.Boolean, numpy.array([
          [1,  1],
          [0,  5],
          [1,  1]], numpy.int8), 5)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (True, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), (False, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)

      with self.assertRaises(Exception) as context_manager:
          raster = pcraster.numpy2pcr(pcraster.Boolean, numpy.array([
              [1,  1],
              [9,  5],
              [1,  1]], numpy.int8), 5)
      self.assertEqual(str(context_manager.exception),
          "Incorrect value 9 at input array [1][0] for Boolean map")

      # int8 -> Ldd (uint8)
      raster = pcraster.numpy2pcr(pcraster.Ldd, numpy.array([
          [1,  2],
          [5,  15],
          [8,  9]], numpy.int8), 15)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (1, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), (5, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)

      with self.assertRaises(Exception) as context_manager:
          raster = pcraster.numpy2pcr(pcraster.Ldd, numpy.array([
              [ 1,  2],
              [10, 15],
              [ 8,  9]], numpy.int8), 15)
      self.assertEqual(str(context_manager.exception),
          "Incorrect value 10 at input array [1][0] for LDD map")

      # int8 -> Nominal (int32)
      # All valid int8 values are valid nominal values.
      raster = pcraster.numpy2pcr(pcraster.Nominal, numpy.array([
          [int8_min,         2],
          [       5,        15],
          [       8,  int8_max]], numpy.int8), 15)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (int8_min, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), (       5, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)
      self.assertEqual(pcraster.cellvalue(raster, 3, 2), (int8_max, True))

      # int8 -> Ordinal (int32)
      # All valid int8 values are valid ordinal values.
      raster = pcraster.numpy2pcr(pcraster.Ordinal, numpy.array([
          [int8_min,         2],
          [       5,        15],
          [       8,  int8_max]], numpy.int8), 15)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (int8_min, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), (       5, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)
      self.assertEqual(pcraster.cellvalue(raster, 3, 2), (int8_max, True))

      # int8 -> Scalar (float32)
      # All valid int8 values are valid scalar values.
      raster = pcraster.numpy2pcr(pcraster.Scalar, numpy.array([
          [int8_min,        2],
          [       5,       15],
          [       8, int8_max]], numpy.int8), 15)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (int8_min, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), (       5, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)
      self.assertEqual(pcraster.cellvalue(raster, 3, 2), (int8_max, True))

      # int8 -> Directional (float32)
      # All valid int8 values are valid directional values.
      raster = pcraster.numpy2pcr(pcraster.Directional, numpy.array([
          [int8_min,        2],
          [       5,       15],
          [       8, int8_max]], numpy.int8), 15)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (int8_min, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), (       5, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)
      self.assertEqual(pcraster.cellvalue(raster, 3, 2), (int8_max, True))

      # int16 TODO
      # int32 TODO

      # int64 -> Boolean (uint8)
      raster = pcraster.numpy2pcr(pcraster.Boolean, numpy.array([
          [1,  1],
          [0,  5],
          [1,  1]], numpy.int64), 5)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (True, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), (False, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)

      with self.assertRaises(Exception) as context_manager:
          raster = pcraster.numpy2pcr(pcraster.Boolean, numpy.array([
              [1,  1],
              [9,  5],
              [1,  1]], numpy.int64), 5)
      self.assertEqual(str(context_manager.exception),
          "Incorrect value 9 at input array [1][0] for Boolean map")

      # int64 -> Ldd (uint8)
      raster = pcraster.numpy2pcr(pcraster.Ldd, numpy.array([
          [1,  2],
          [5,  15],
          [8,  9]], numpy.int64), 15)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (1, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), (5, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)

      with self.assertRaises(Exception) as context_manager:
          raster = pcraster.numpy2pcr(pcraster.Ldd, numpy.array([
              [ 1,  2],
              [10, 15],
              [ 8,  9]], numpy.int64), 15)
      self.assertEqual(str(context_manager.exception),
          "Incorrect value 10 at input array [1][0] for LDD map")

      # int64 -> Nominal (int32)
      raster = pcraster.numpy2pcr(pcraster.Nominal, numpy.array([
          [int32_min+1,         2],
          [          5,        15],
          [          8, int32_max]], numpy.int64), 15)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (int32_min + 1, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), (            5, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)
      self.assertEqual(pcraster.cellvalue(raster, 3, 2), (    int32_max, True))

      with self.assertRaises(Exception) as context_manager:
          raster = pcraster.numpy2pcr(pcraster.Nominal, numpy.array([
              [int32_min+1,         2],
              [  int32_min,        15],
              [          8, int32_max]], numpy.int64), 15)
      self.assertEqual(str(context_manager.exception),
          "Incorrect value -2147483648 at input array [1][0] for Nominal map")

      # uint8 TODO
      # uint16 TODO
      # uint32 TODO
      # uint64 TODO

      # float16 TODO

      # float32 -> Scalar (float32)
      raster = pcraster.numpy2pcr(pcraster.Scalar, numpy.array([
          [-2,        -1],
          [ 0, numpy.nan],
          [ 1,         2]], numpy.float32), numpy.nan)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (-2, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), ( 0, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)
      self.assertEqual(pcraster.cellvalue(raster, 3, 2),  (2, True))

      # float64 -> Scalar (float32)
      raster = pcraster.numpy2pcr(pcraster.Scalar, numpy.array([
          [-2,        -1],
          [ 0, numpy.nan],
          [ 1,         2]], numpy.float64), numpy.nan)
      self.assertEqual(pcraster.cellvalue(raster, 1, 1), (-2, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 1), ( 0, True))
      self.assertEqual(pcraster.cellvalue(raster, 2, 2)[1], False)
      self.assertEqual(pcraster.cellvalue(raster, 3, 2),  (2, True))

      # complex64: Not supported.
      with self.assertRaises(Exception) as context_manager:
          raster = pcraster.numpy2pcr(pcraster.Nominal, numpy.array([
          [-2, -1],
          [ 0, 15],
          [ 1,  2]], numpy.complex64), 15)
      self.assertEqual(str(context_manager.exception),
          "Unsupported array type")

      # Illegal:
      # - float96/128 TODO
      # - complex64/192/256 TODO
      # ...


  def test_001(self):
      """ nonspatial and pcr2numpy """
      nrRows, nrCols, cellSize = 5, 8, 1.0
      west, north = 0.0, 0.0
      pcraster.setclone(nrRows, nrCols, cellSize, west, north)

      value = 1.23456
      nonspatial = pcraster.scalar(value)
      array = pcraster.pcr2numpy(nonspatial, numpy.nan)

      for row in range(0, nrRows):
          for col in range(0, nrCols):
              self.assertAlmostEqual(array[row][col], value)

      value = 3
      nonspatial = pcraster.nominal(value)
      array = pcraster.pcr2numpy(nonspatial, numpy.nan)

      for row in range(0, nrRows):
          for col in range(0, nrCols):
              self.assertAlmostEqual(array[row][col], value)

      value = True
      nonspatial = pcraster.boolean(value)
      array = pcraster.pcr2numpy(nonspatial, numpy.nan)

      for row in range(0, nrRows):
          for col in range(0, nrCols):
              self.assertAlmostEqual(array[row][col], value)


  def test_002(self):
      """ nonspatial and pcr_as_numpy """
      nrRows, nrCols, cellSize = 3, 2, 1.0
      west, north = 0.0, 0.0
      pcraster.setclone(nrRows, nrCols, cellSize, west, north)

      nonspatial = pcraster.nominal(5)

      with self.assertRaises(Exception) as context_manager:
          array = pcraster.pcr_as_numpy(nonspatial)

      self.assertEqual(str(context_manager.exception),
          "Argument is non-spatial, only spatial PCRaster data types are supported")
