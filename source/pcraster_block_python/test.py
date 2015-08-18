#!/usr/bin/env python

import unittest
import usecasetest

suite = unittest.TestSuite()
suite.addTest(unittest.makeSuite( \
         usecasetest.UseCaseTest))
unittest.TextTestRunner().run(suite)
