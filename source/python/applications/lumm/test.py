#!/usr/bin/env python

import unittest
import tasktest, utilstest

suite = unittest.TestSuite()
suite.addTest(unittest.makeSuite( \
         tasktest.TaskTest))
# suite.addTest(unittest.makeSuite( \
#          tasktest.CompositeTaskTest))
suite.addTest(unittest.makeSuite( \
         utilstest.UtilsTest))
unittest.TextTestRunner().run(suite)
