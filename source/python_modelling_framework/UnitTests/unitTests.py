#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
import unittest
try:
  import frameworkBaseTest
  import staticFrameworkTest
  import dynamicFrameworkTest
  import mcFrameworkTest
  import particleFilterFrameworkTest
  import PercentileTests
  import TimeoutputTest
except ImportError as error:
  # leave the print statement here to ensure some useful
  # error message shows up while running ctest
  print(error)
  raise SystemExit(error)

suites = []
suites.append(unittest.TestLoader().loadTestsFromTestCase(PercentileTests.PercentileTests))
suites.append(unittest.TestLoader().loadTestsFromTestCase(frameworkBaseTest.frameworkBaseTestScript))
suites.append(unittest.TestLoader().loadTestsFromTestCase(staticFrameworkTest.staticFrameworkTestScript))
suites.append(unittest.TestLoader().loadTestsFromTestCase(dynamicFrameworkTest.dynamicFrameworkTestScript))
suites.append(unittest.TestLoader().loadTestsFromTestCase(mcFrameworkTest.mcFrameworkTestScript))
suites.append(unittest.TestLoader().loadTestsFromTestCase(particleFilterFrameworkTest.ParticleFilterFrameworkTestScript))
suites.append(unittest.TestLoader().loadTestsFromTestCase(TimeoutputTest.TimeoutputTest))
suites = unittest.TestSuite(suites)

result = unittest.TextTestRunner(verbosity=3).run(suites)
test_result = (0 if result.wasSuccessful() else 1)

sys.exit(test_result)
