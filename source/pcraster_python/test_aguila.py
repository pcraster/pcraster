import os
import pathlib
import sys
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
            pcraster.aguila('--timesteps=[1, 10] q', pcraster_unit_test=True)
        self.assertEqual(str(context_manager.exception), f'aguila --timesteps=[1, 10] q')


    def test_02(self):
        """ aguila executed in directories without write permission """

        if sys.platform.startswith('linux'):
          test_path = pathlib.Path('/usr/')
          expected = 'aguila /tmp/tmp'
        elif sys.platform.startswith('win32'):
          test_path = pathlib.Path('C:', 'Windows')
          tmp = os.environ['TEMP']
          expected = f'aguila.exe {tmp}'
        elif sys.platform.startswith('darwin'):
          test_path = pathlib.Path('/System/')
          tmp = os.environ['TMPDIR']
          expected = f'aguila {tmp}'

        os.chdir(test_path)
        pcraster.setclone(5, 4, 3, 2, 1)

        with self.assertRaisesRegex(Exception, expected):
            pcraster.aguila(pcraster.uniform(1), pcraster_unit_test=True)
