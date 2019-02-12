import os
import unittest
import testcase
import pickle
import pcraster


class TestClass(object):
  def __init__(self):
    self.val = 5.3
    self.field = pcraster.readmap(os.path.join("validated", "sin_Result.map"))


class TestPickle(testcase.TestCase):
  def test_01(self):
    """ pickle PCRaster maps of all data types """
    no_assertion_raised = True
    try:
      raster_boolean = pcraster.readmap(os.path.join("validated", "boolean_Result.map"))
      pickle.dump(raster_boolean, open("pickle_boolean.pkl", "wb"))

      raster_nominal = pcraster.readmap(os.path.join("validated", "nominal_Result.map"))
      pickle.dump(raster_nominal, open("pickle_nominal.pkl", "wb"))

      raster_ordinal = pcraster.readmap(os.path.join("validated", "ordinal_Result.map"))
      pickle.dump(raster_ordinal, open("pickle_ordinal.pkl", "wb"))

      raster_scalar = pcraster.readmap(os.path.join("validated", "sin_Result.map"))
      pickle.dump(raster_scalar, open("pickle_scalar.pkl", "wb"))

      raster_direction = pcraster.readmap(os.path.join("validated", "directional_Result1.map"))
      pickle.dump(raster_direction, open("pickle_direction.pkl", "wb"))

      raster_ldd = pcraster.readmap(os.path.join("validated", "ldd_Result.map"))
      pickle.dump(raster_ldd, open("pickle_ldd.pkl", "wb"))
    except Exception as e:
      print(e)
      no_assertion_raised = False

    self.assertTrue(no_assertion_raised)


  def test_02(self):
    """ unpickle boolean """
    field_pkl = pickle.load(open("pickle_boolean.pkl", "rb"))
    pcraster.report(field_pkl, "pickle_boolean.map")
    self.assertTrue(self.mapEqualsValidated(field_pkl, "boolean_Result.map"))


  def test_03(self):
    """ unpickle nominal """
    field_pkl = pickle.load(open("pickle_nominal.pkl", "rb"))
    pcraster.report(field_pkl, "pickle_nominal.map")
    self.assertTrue(self.mapEqualsValidated(field_pkl, "nominal_Result.map"))


  def test_04(self):
    """ unpickle ordinal """
    field_pkl = pickle.load(open("pickle_ordinal.pkl", "rb"))
    pcraster.report(field_pkl, "pickle_ordinal.map")
    self.assertTrue(self.mapEqualsValidated(field_pkl, "ordinal_Result.map"))


  def test_05(self):
    """ unpickle scalar """
    field_pkl = pickle.load(open("pickle_scalar.pkl", "rb"))
    pcraster.report(field_pkl, "pickle_scalar.map")
    self.assertTrue(self.mapEqualsValidated(field_pkl, "sin_Result.map"))


  def test_06(self):
    """ unpickle directional """
    field_pkl = pickle.load(open("pickle_direction.pkl", "rb"))
    pcraster.report(field_pkl, "pickle_direction.map")
    self.assertTrue(self.mapEqualsValidated(field_pkl, "directional_Result1.map"))


  def test_07(self):
    """ unpickle ldd """
    field_pkl = pickle.load(open("pickle_ldd.pkl", "rb"))
    pcraster.report(field_pkl, "pickle_ldd.map")
    self.assertTrue(self.mapEqualsValidated(field_pkl, "ldd_Result.map"))


  def test_08(self):
    """ pickle class """
    no_assertion_raised = True
    try:
      p = TestClass()
      pickle.dump(p, open("pickle_class.pkl", "wb"))
    except Exception as e:
      no_assertion_raised = False

    self.assertTrue(no_assertion_raised)


  def test_09(self):
    """ unpickle class """
    p = pickle.load(open("pickle_class.pkl", "rb"))
    self.assertEqual(p.val, 5.3)
    self.assertTrue(self.mapEqualsValidated(p.field, "sin_Result.map"))


  def test_10(self):
    """ unpickled differs from clonemap """
    pcraster.setclone(25, 20, 123.4, 20, -25)

    with self.assertRaises(Exception) as context_manager:
      field_pkl = pickle.load(open("pickle_nominal.pkl", "rb"))
    self.assertEqual(str(context_manager.exception), "number of rows and columns (3, 3) differ from currently used (25, 20)\n")

    pcraster.setclone(3, 3, 123.4, 20, -25)

    with self.assertRaises(Exception) as context_manager:
      field_pkl = pickle.load(open("pickle_nominal.pkl", "rb"))
    self.assertEqual(str(context_manager.exception), "west and north (0, 0) differ from currently used (20, -25)\n")

    pcraster.setclone(3, 3, 123.4, 0, 0)

    with self.assertRaises(Exception) as context_manager:
      field_pkl = pickle.load(open("pickle_nominal.pkl", "rb"))
    self.assertEqual(str(context_manager.exception), "cell size (1) differs from currently used (123.4)\n")
