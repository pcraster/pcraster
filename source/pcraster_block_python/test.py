#!/usr/bin/env python
import unittest

try:
  import usecasetest
except ImportError as error:
  # leave the print statement here to ensure some useful
  # error message shows up while running ctest
  print(error)
  raise SystemExit(error)


suite = unittest.TestSuite()
suite.addTest(unittest.makeSuite( \
         usecasetest.UseCaseTest))

result = unittest.TextTestRunner(verbosity=2).run(suite)
test_result = (0 if result.wasSuccessful() else 1)
