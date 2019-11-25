#
# These tests are done for the pcraster and multicore modules
#
import os
import unittest
import testcase
import pickle
import pcraster


class TestPCRaster(testcase.TestCase):
    def test_01(self):
        """  divide float by field """
        raster = pcraster.readmap("abs_Expr.map")
        result = 1 / raster

        value, isValid = pcraster.cellvalue(result, 1)
        self.assertEqual(isValid, True)
        self.assertAlmostEqual(value, 0.5)
        value, isValid = pcraster.cellvalue(result, 2)
        self.assertEqual(isValid, True)
        self.assertAlmostEqual(value, -0.142857, 6)
        value, isValid = pcraster.cellvalue(result, 3)
        self.assertEqual(isValid, True)
        self.assertAlmostEqual(value, 0.285714, 6)
        value, isValid = pcraster.cellvalue(result, 4)
        self.assertEqual(isValid, True)
        self.assertAlmostEqual(value, -0.117647, 6)
        value, isValid = pcraster.cellvalue(result, 5)
        self.assertEqual(isValid, True)
        self.assertAlmostEqual(value, 0.277778, 6)
        value, isValid = pcraster.cellvalue(result, 6)
        self.assertEqual(isValid, False)
        value, isValid = pcraster.cellvalue(result, 7)
        self.assertEqual(isValid, False)
        value, isValid = pcraster.cellvalue(result, 8)
        self.assertEqual(isValid, True)
        self.assertAlmostEqual(value, 0.0714286, 6)
        value, isValid = pcraster.cellvalue(result, 9)
        self.assertEqual(isValid, True)
        self.assertAlmostEqual(value, -1.25)


    def test_02(self):
        """  POD condition in ifthen """
        nr_rows = 2
        nr_cols = 3
        nr_cells = nr_rows * nr_cols
        pcraster.setclone(nr_rows, nr_cols, 5, 1, 1)

        raster = pcraster.ifthen(1, pcraster.scalar(4.567))

        for idx in range(1, nr_cells + 1):
          value, isValid = pcraster.cellvalue(raster, idx)
          self.assertEqual(isValid, True)
          self.assertAlmostEqual(value, 4.567, 6)

        raster = pcraster.ifthen(0, pcraster.scalar(4.567))

        for idx in range(1, nr_cells + 1):
          value, isValid = pcraster.cellvalue(raster, idx)
          self.assertEqual(isValid, False)



    def test_03(self):
        """  nonspatial condition in ifthen """
        nr_rows = 2
        nr_cols = 3
        nr_cells = nr_rows * nr_cols
        pcraster.setclone(nr_rows, nr_cols, 5, 1, 1)


        raster = pcraster.ifthen(pcraster.boolean(1), pcraster.scalar(4.567))

        for idx in range(1, nr_cells + 1):
          value, isValid = pcraster.cellvalue(raster, idx)
          self.assertEqual(isValid, True)
          self.assertAlmostEqual(value, 4.567, 6)

        raster = pcraster.ifthen(pcraster.boolean(0), pcraster.scalar(4.567))

        for idx in range(1, nr_cells + 1):
          value, isValid = pcraster.cellvalue(raster, idx)
          self.assertEqual(isValid, False)


        raster = pcraster.ifthen(pcraster.scalar(1), pcraster.scalar(4.567))

        for idx in range(1, nr_cells + 1):
          value, isValid = pcraster.cellvalue(raster, idx)
          self.assertEqual(isValid, True)
          self.assertAlmostEqual(value, 4.567, 6)


        raster = pcraster.ifthen(pcraster.scalar(0), pcraster.scalar(4.567))

        for idx in range(1, nr_cells + 1):
          value, isValid = pcraster.cellvalue(raster, idx)
          self.assertEqual(isValid, False)
