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
