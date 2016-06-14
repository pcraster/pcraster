#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
import unittest
try:
  import IndexTest
  import VariableCollectionTest
except ImportError as error:
  # leave the print statement here to ensure some useful
  # error message shows up while running ctest
  print(error)
  raise SystemExit(error)

suites = []
suites.append(unittest.makeSuite(IndexTest.IndexUnitTests))
suites.append(unittest.makeSuite(VariableCollectionTest.CollectionUnitTests))
suites = unittest.TestSuite(suites)

result = unittest.TextTestRunner(verbosity=3).run(suites)
test_result = (0 if result.wasSuccessful() else 1)

sys.exit(test_result)

