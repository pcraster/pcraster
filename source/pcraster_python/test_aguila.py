import pathlib
import unittest

import pcraster
import testcase


class TestAguila(testcase.TestCase):

    def test_01(self):
        """ mapping of function arguments to command line """

        p1 = str(pathlib.Path('test_path', 'ldd_Result.map'))
        p2 = str(pathlib.Path('test_path', 'cos_Result.map'))
        with self.assertRaises(Exception) as context_manager:
            pcraster.aguila(p1, p2, pcraster_unit_test=True)
        self.assertEqual(str(context_manager.exception), f'aguila {p1} {p2}')


        p1 = str(pathlib.Path('test_path', 'ldd Result.map'))
        with self.assertRaises(Exception) as context_manager:
            pcraster.aguila(p1, pcraster_unit_test=True)
        self.assertEqual(str(context_manager.exception), f'aguila "{p1}"')


        p1 = str(pathlib.Path('test path', 'ldd Result.map'))
        p2 = str(pathlib.Path('test_path', 'cos_Result.map'))
        p3 = str(pathlib.Path('test path', 'cos_Result.map'))
        with self.assertRaises(Exception) as context_manager:
            pcraster.aguila(p1, p2, p3, pcraster_unit_test=True)
        self.assertEqual(str(context_manager.exception), f'aguila "{p1}" {p2} "{p3}"')


        with self.assertRaises(Exception) as context_manager:
            pcraster.aguila([p1, p2], p3, pcraster_unit_test=True)
        self.assertEqual(str(context_manager.exception), f'aguila "{p1}" + {p2} "{p3}"')


        with self.assertRaises(Exception) as context_manager:
            pcraster.aguila(p1, [p2, p3], pcraster_unit_test=True)
        self.assertEqual(str(context_manager.exception), f'aguila "{p1}" {p2} + "{p3}"')


        with self.assertRaises(Exception) as context_manager:
            pcraster.aguila('--timesteps=[1, 10] q', pcraster_unit_test=False)
        self.assertEqual(str(context_manager.exception), f'aguila --timesteps=[1, 10] q')
