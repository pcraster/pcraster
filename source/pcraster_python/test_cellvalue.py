import unittest
import math

import pcraster
import testcase


class CellvalueTest(testcase.TestCase):

    def _read_set_clone(self, filename):
        pcraster.setclone(filename)
        return pcraster.readmap(filename)

    def testReadFieldCell(self):
        value, mv = pcraster.readFieldCell("abs_Expr.map", 1, 1)
        self.assertEqual(value, 2.0)
        self.assertEqual(mv, True)
        value, mv = pcraster.readFieldCell("abs_Expr.map", 1, 2)
        self.assertEqual(value, -7.0)
        self.assertEqual(mv, True)
        value, mv = pcraster.readFieldCell("abs_Expr.map", 1, 3)
        self.assertEqual(value, 3.5)
        self.assertEqual(mv, True)
        value, mv = pcraster.readFieldCell("abs_Expr.map", 2, 1)
        self.assertEqual(value, -8.5)
        self.assertEqual(mv, True)
        value, mv = pcraster.readFieldCell("abs_Expr.map", 2, 2)
        self.assertAlmostEqual(value, 3.6, 6)
        self.assertEqual(mv, True)
        value, mv = pcraster.readFieldCell("abs_Expr.map", 2, 3)
        self.assertEqual(mv, False)
        value, mv = pcraster.readFieldCell("abs_Expr.map", 3, 1)
        self.assertEqual(value, 0.0)
        self.assertEqual(mv, True)
        value, mv = pcraster.readFieldCell("abs_Expr.map", 3, 2)
        self.assertEqual(value, 14.0)
        self.assertEqual(mv, True)
        value, mv = pcraster.readFieldCell("abs_Expr.map", 3, 3)
        self.assertAlmostEqual(value, -0.8)
        self.assertEqual(mv, True)

    def valueTest(self, readValue, readValidValue, trueValidValue, type, trueValue):
        self.assertTrue(isinstance(readValidValue, int))
        self.assertEqual(readValidValue, trueValidValue)
        if readValidValue:
            self.assertTrue(isinstance(readValue, type))
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
        raster = self._read_set_clone("areaarea_Class.map")
        value, isValid = pcraster.cellvalue(raster, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 2)
        value, isValid = pcraster.cellvalue(raster, 2)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 6)
        value, isValid = pcraster.cellvalue(raster, 5)
        self.assertEqual(isValid, False)
        value, isValid = pcraster.cellvalue(raster, 9)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 2)
        value, isValid = pcraster.cellvalue(raster, 25)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 4)

    def testCellValueOrdinal(self):
        raster = pcraster.ordinal(self._read_set_clone("areaarea_Class.map"))
        value, isValid = pcraster.cellvalue(raster, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 2)
        value, isValid = pcraster.cellvalue(raster, 2)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 6)
        value, isValid = pcraster.cellvalue(raster, 5)
        self.assertEqual(isValid, False)

    def testCellValueScalar(self):
        raster = self._read_set_clone("abs_Expr.map")
        value, isValid = pcraster.cellvalue(raster, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 2.0)
        value, isValid = pcraster.cellvalue(raster, 2)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, -7.0)
        value, isValid = pcraster.cellvalue(raster, 3)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 3.5)
        value, isValid = pcraster.cellvalue(raster, 6)
        self.assertEqual(isValid, False)
        value, isValid = pcraster.cellvalue(raster, 7)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 0.0)
        value, isValid = pcraster.cellvalue(raster, 8)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 14.0)

    def testCellValueDirectional(self):
        raster = self._read_set_clone("nodirection_Expr.map")
        value, isValid = pcraster.cellvalue(raster, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertAlmostEqual(value, math.radians(280))
        value, isValid = pcraster.cellvalue(raster, 2)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertAlmostEqual(value, math.radians(25))
        value, isValid = pcraster.cellvalue(raster, 5)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertAlmostEqual(value, -1.0)
        value, isValid = pcraster.cellvalue(raster, 7)
        self.assertEqual(isValid, False)
        value, isValid = pcraster.cellvalue(raster, 9)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertAlmostEqual(value, math.radians(7))

    def testCellValueLdd(self):
        raster = self._read_set_clone("accu_Ldd.map")
        value, isValid = pcraster.cellvalue(raster, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 2)
        value, isValid = pcraster.cellvalue(raster, 2)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 2)
        value, isValid = pcraster.cellvalue(raster, 9)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 1)
        value, isValid = pcraster.cellvalue(raster, 22)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 5)
        value, isValid = pcraster.cellvalue(raster, 25)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 4)

    def testCellValueNonSpatial(self):
        raster = self._read_set_clone("abs_Expr.map")
        value, isValid = pcraster.cellvalue(pcraster.mapmaximum(raster), 1, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 14.0)
        value, isValid = pcraster.cellvalue(pcraster.mapmaximum(raster), 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 14.0)

    def test_01(self):
        """ cellvalue_by_index Boolean """
        raster = pcraster.readmap("and_Expr1.map")
        value, isValid = pcraster.cellvalue_by_index(raster, 0)
        self.valueTest(value, isValid, True, int, True)
        value, isValid = pcraster.cellvalue_by_index(raster, 1)
        self.valueTest(value, isValid, True, int, True)
        value, isValid = pcraster.cellvalue_by_index(raster, 2)
        self.valueTest(value, isValid, True, int, False)
        value, isValid = pcraster.cellvalue_by_index(raster, 3)
        self.valueTest(value, isValid, True, int, False)
        value, isValid = pcraster.cellvalue_by_index(raster, 4)
        self.valueTest(value, isValid, False, None, None)
        value, isValid = pcraster.cellvalue_by_index(raster, 5)
        self.valueTest(value, isValid, True, int, False)
        value, isValid = pcraster.cellvalue_by_index(raster, 6)
        self.valueTest(value, isValid, True, int, True)
        value, isValid = pcraster.cellvalue_by_index(raster, 7)
        self.valueTest(value, isValid, True, int, True)
        value, isValid = pcraster.cellvalue_by_index(raster, 8)
        self.valueTest(value, isValid, True, int, False)

    def test_02(self):
        """ cellvalue_by_index Nominal """
        raster = self._read_set_clone("areaarea_Class.map")
        value, isValid = pcraster.cellvalue_by_index(raster, 0)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 2)
        value, isValid = pcraster.cellvalue_by_index(raster, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 6)
        value, isValid = pcraster.cellvalue_by_index(raster, 4)
        self.assertEqual(isValid, False)
        value, isValid = pcraster.cellvalue_by_index(raster, 8)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 2)
        value, isValid = pcraster.cellvalue_by_index(raster, 24)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 4)

    def test_03(self):
        """ cellvalue_by_index Ordinal """
        raster = pcraster.ordinal(self._read_set_clone("areaarea_Class.map"))
        value, isValid = pcraster.cellvalue_by_index(raster, 0)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 2)
        value, isValid = pcraster.cellvalue_by_index(raster, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 6)
        value, isValid = pcraster.cellvalue_by_index(raster, 4)
        self.assertEqual(isValid, False)

    def test_04(self):
        """ cellvalue_by_index Scalar """
        raster = self._read_set_clone("abs_Expr.map")
        value, isValid = pcraster.cellvalue_by_index(raster, 0)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 2.0)
        value, isValid = pcraster.cellvalue_by_index(raster, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, -7.0)
        value, isValid = pcraster.cellvalue_by_index(raster, 2)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 3.5)
        value, isValid = pcraster.cellvalue_by_index(raster, 5)
        self.assertEqual(isValid, False)
        value, isValid = pcraster.cellvalue_by_index(raster, 6)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 0.0)
        value, isValid = pcraster.cellvalue_by_index(raster, 7)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 14.0)

    def test_05(self):
        """ cellvalue_by_index Directional """
        raster = self._read_set_clone("nodirection_Expr.map")
        value, isValid = pcraster.cellvalue_by_index(raster, 0)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertAlmostEqual(value, math.radians(280))
        value, isValid = pcraster.cellvalue_by_index(raster, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertAlmostEqual(value, math.radians(25))
        value, isValid = pcraster.cellvalue_by_index(raster, 4)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertAlmostEqual(value, -1.0)
        value, isValid = pcraster.cellvalue_by_index(raster, 6)
        self.assertEqual(isValid, False)
        value, isValid = pcraster.cellvalue_by_index(raster, 8)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertAlmostEqual(value, math.radians(7))

    def test_06(self):
        """ cellvalue_by_index Ldd"""
        raster = self._read_set_clone("accu_Ldd.map")
        value, isValid = pcraster.cellvalue_by_index(raster, 0)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 2)
        value, isValid = pcraster.cellvalue_by_index(raster, 1)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 2)
        value, isValid = pcraster.cellvalue_by_index(raster, 8)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 1)
        value, isValid = pcraster.cellvalue_by_index(raster, 21)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 5)
        value, isValid = pcraster.cellvalue_by_index(raster, 24)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, int))
        self.assertEqual(value, 4)

    def test_07(self):
        """ cellvalue_by_index NonSpatial """
        raster = self._read_set_clone("abs_Expr.map")
        value, isValid = pcraster.cellvalue_by_indices(pcraster.mapmaximum(raster), 0, 0)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 14.0)
        value, isValid = pcraster.cellvalue_by_index(pcraster.mapmaximum(raster), 0)
        self.assertEqual(isValid, True)
        self.assertTrue(isinstance(value, float))
        self.assertEqual(value, 14.0)

    def test_08(self):
        """ cellvalue_by exceptions """

        raster = self._read_set_clone("abs_Expr.map")

        with self.assertRaises(Exception) as context_manager:
            value, isValid = pcraster.cellvalue_by_index(raster, 9)
        self.assertEqual(str(context_manager.exception), "cellvalue index '9' out of range [0, 8]")

        with self.assertRaises(Exception) as context_manager:
            value, isValid = pcraster.cellvalue_by_indices(raster, 9, 0)
        self.assertEqual(str(context_manager.exception), "cellvalue row index '9' out of range [0, 2]")

        with self.assertRaises(Exception) as context_manager:
            value, isValid = pcraster.cellvalue_by_indices(raster, 0, 9)
        self.assertEqual(str(context_manager.exception), "cellvalue column index '9' out of range [0, 2]")

    def test_09(self):
        """ cellvalue by coordinates """

        pcraster.setclone(4, 5, 12.3, -20, 20)

        raster = pcraster.uniform(1)

        value, isValid = pcraster.cellvalue_by_coordinates(raster, -4.625, 12.7396)
        self.assertAlmostEqual(value, 0.295846, places=6)

        value, isValid = pcraster.cellvalue_by_coordinates(raster, 23.7333, 2.66042)
        self.assertAlmostEqual(value, 0.886274, places=6)

        value, isValid = pcraster.cellvalue_by_coordinates(raster, -13.3375, -23.1354)
        self.assertAlmostEqual(value, 0.315315, places=6)

        value, isValid = pcraster.cellvalue_by_coordinates(raster, 40.6458, -5.71042)
        self.assertAlmostEqual(value, 0.553938, places=6)

    def test_10(self):
        """ cellvalue, coordinates exceptions """

        pcraster.setclone(4, 5, 12.3, -20, 20)

        raster = pcraster.uniform(1)

        with self.assertRaises(Exception) as context_manager:
            value, isValid = pcraster.cellvalue_by_coordinates(raster, -23.3375, -23.1354)
        self.assertEqual(str(context_manager.exception), "xcoordinate '-23.3375' out of range [-20, 41.5]")

        with self.assertRaises(Exception) as context_manager:
            value, isValid = pcraster.cellvalue_by_coordinates(raster, 43.3375, -23.1354)
        self.assertEqual(str(context_manager.exception), "xcoordinate '43.3375' out of range [-20, 41.5]")

        with self.assertRaises(Exception) as context_manager:
            value, isValid = pcraster.cellvalue_by_coordinates(raster, -13.3375, 23.1354)
        self.assertEqual(str(context_manager.exception), "ycoordinate '23.1354' out of range [20, -29.2]")

        with self.assertRaises(Exception) as context_manager:
            value, isValid = pcraster.cellvalue_by_coordinates(raster, -13.3375, -33.1354)
        self.assertEqual(str(context_manager.exception), "ycoordinate '-33.1354' out of range [20, -29.2]")
