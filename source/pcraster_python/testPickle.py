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
  def test_1(self):
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
      no_assertion_raised = False

    self.assert_(no_assertion_raised)


  def test_2(self):
    """ unpickle boolean """
    field_pkl = pickle.load(open("pickle_boolean.pkl", "rb"))
    pcraster.report(field_pkl, "bla.map")
    self.failUnless(self.mapEqualsValidated(field_pkl, "boolean_Result.map"))


  def test_3(self):
    """ unpickle nominal """
    field_pkl = pickle.load(open("pickle_nominal.pkl", "rb"))
    pcraster.report(field_pkl, "bla_nominal.map")
    self.failUnless(self.mapEqualsValidated(field_pkl, "nominal_Result.map"))


  def test_4(self):
    """ unpickle ordinal """
    field_pkl = pickle.load(open("pickle_ordinal.pkl", "rb"))
    pcraster.report(field_pkl, "bla_ordinal.map")
    self.failUnless(self.mapEqualsValidated(field_pkl, "ordinal_Result.map"))


  def test_5(self):
    """ unpickle scalar """
    field_pkl = pickle.load(open("pickle_scalar.pkl", "rb"))
    pcraster.report(field_pkl, "bla_scalar.map")
    self.failUnless(self.mapEqualsValidated(field_pkl, "sin_Result.map"))


  def test_6(self):
    """ unpickle directional """
    field_pkl = pickle.load(open("pickle_direction.pkl", "rb"))
    pcraster.report(field_pkl, "bla_direction.map")
    self.failUnless(self.mapEqualsValidated(field_pkl, "directional_Result1.map"))


  def test_7(self):
    """ unpickle ldd """
    field_pkl = pickle.load(open("pickle_ldd.pkl", "rb"))
    self.failUnless(self.mapEqualsValidated(field_pkl, "ldd_Result.map"))


  def test_8(self):
    """ pickle class """
    no_assertion_raised = True
    try:
      p = TestClass()
      pickle.dump(p, open("pickle_class.pkl", "wb"))
    except Exception as e:
      no_assertion_raised = False

    self.assert_(no_assertion_raised)


  def test_9(self):
    """ unpickle class """
    p = pickle.load(open("pickle_class.pkl", "rb"))
    self.assertEqual(p.val, 5.3)
    self.failUnless(self.mapEqualsValidated(p.field, "sin_Result.map"))
