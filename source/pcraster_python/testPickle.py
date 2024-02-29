import os
import pickle

import pcraster
import testcase


class TestClass(object):
    def __init__(self):
        self.val = 5.3
        self.field = pcraster.readmap(os.path.join("validated", "sin_Result.map"))


class TestPickle(testcase.TestCase):
    def test_01(self):
        """ pickle PCRaster maps of all data types """
        assertion_raised = False
        try:
            raster_boolean = pcraster.readmap(os.path.join("validated", "boolean_Result.map"))
            with open("pickle_boolean.pkl", 'wb') as f:
                pickle.dump(raster_boolean, f, pickle.HIGHEST_PROTOCOL)

            raster_nominal = pcraster.readmap(os.path.join("validated", "nominal_Result.map"))
            with open("pickle_nominal.pkl", 'wb') as f:
                pickle.dump(raster_nominal, f, pickle.HIGHEST_PROTOCOL)

            raster_ordinal = pcraster.readmap(os.path.join("validated", "ordinal_Result.map"))
            with open("pickle_ordinal.pkl", 'wb') as f:
                pickle.dump(raster_ordinal, f, pickle.HIGHEST_PROTOCOL)

            raster_scalar = pcraster.readmap(os.path.join("validated", "sin_Result.map"))
            with open("pickle_scalar.pkl", 'wb') as f:
                pickle.dump(raster_scalar, f, pickle.HIGHEST_PROTOCOL)

            raster_direction = pcraster.readmap(os.path.join("validated", "directional_Result1.map"))
            with open("pickle_direction.pkl", 'wb') as f:
                pickle.dump(raster_direction, f, pickle.HIGHEST_PROTOCOL)

            raster_ldd = pcraster.readmap(os.path.join("validated", "ldd_Result.map"))
            with open("pickle_ldd.pkl", 'wb') as f:
                pickle.dump(raster_ldd, f, pickle.HIGHEST_PROTOCOL)
        except pickle.PickleError as ex:
            print(ex, flush=True)
            assertion_raised = True

        self.assertFalse(assertion_raised)

    def test_02(self):
        """ unpickle boolean """
        assertion_raised = False
        try:
            with open("pickle_boolean.pkl", "rb") as f:
                field_pkl = pickle.load(f)
                pcraster.report(field_pkl, "pickle_boolean.map")
                self.assertTrue(self.mapEqualsValidated(field_pkl, "boolean_Result.map"))
        except pickle.PickleError as ex:
            print(ex, flush=True)
            assertion_raised = True

        self.assertFalse(assertion_raised)

    def test_03(self):
        """ unpickle nominal """
        assertion_raised = False
        try:
            with open("pickle_nominal.pkl", "rb") as f:
                field_pkl = pickle.load(f)
                pcraster.report(field_pkl, "pickle_nominal.map")
                self.assertTrue(self.mapEqualsValidated(field_pkl, "nominal_Result.map"))
        except pickle.PickleError as ex:
            print(ex, flush=True)
            assertion_raised = True

        self.assertFalse(assertion_raised)

    def test_04(self):
        """ unpickle ordinal """
        assertion_raised = False
        try:
            with open("pickle_ordinal.pkl", "rb") as f:
                field_pkl = pickle.load(f)
                pcraster.report(field_pkl, "pickle_ordinal.map")
                self.assertTrue(self.mapEqualsValidated(field_pkl, "ordinal_Result.map"))
        except pickle.PickleError as ex:
            print(ex, flush=True)
            assertion_raised = True

        self.assertFalse(assertion_raised)

    def test_05(self):
        """ unpickle scalar """
        assertion_raised = False
        try:
            with open("pickle_scalar.pkl", "rb") as f:
                field_pkl = pickle.load(f)
                pcraster.report(field_pkl, "pickle_scalar.map")
                self.assertTrue(self.mapEqualsValidated(field_pkl, "sin_Result.map"))
        except pickle.PickleError as ex:
            print(ex, flush=True)
            assertion_raised = True

        self.assertFalse(assertion_raised)

    def test_06(self):
        """ unpickle directional """
        assertion_raised = False
        try:
            with open("pickle_direction.pkl", "rb") as f:
                field_pkl = pickle.load(f)
                pcraster.report(field_pkl, "pickle_direction.map")
                self.assertTrue(self.mapEqualsValidated(field_pkl, "directional_Result1.map"))
        except pickle.PickleError as ex:
            print(ex, flush=True)
            assertion_raised = True

        self.assertFalse(assertion_raised)

    def test_07(self):
        """ unpickle ldd """
        assertion_raised = False
        try:
            with open("pickle_ldd.pkl", "rb") as f:
                field_pkl = pickle.load(f)
                pcraster.report(field_pkl, "pickle_ldd.map")
                self.assertTrue(self.mapEqualsValidated(field_pkl, "ldd_Result.map"))
        except pickle.PickleError as ex:
            print(ex, flush=True)
            assertion_raised = True

        self.assertFalse(assertion_raised)

    def test_08(self):
        """ pickle class """
        assertion_raised = False
        try:
            p = TestClass()
            with open("pickle_class.pkl", "wb") as f:
                pickle.dump(p, f)
        except pickle.PickleError as ex:
            print(ex, flush=True)
            assertion_raised = True

        self.assertFalse(assertion_raised)

    def test_09(self):
        """ unpickle class """
        assertion_raised = False
        try:
            with open("pickle_class.pkl", "rb") as f:
                p = pickle.load(f)
                self.assertEqual(p.val, 5.3)
                self.assertTrue(self.mapEqualsValidated(p.field, "sin_Result.map"))
        except pickle.PickleError as ex:
            print(ex, flush=True)
            assertion_raised = True

        self.assertFalse(assertion_raised)

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
