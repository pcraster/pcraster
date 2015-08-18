import unittest

import  PCRasterLinkOutTest

suites = []
suites.append(unittest.makeSuite(PCRasterLinkOutTest.PCRasterLinkOutTest))
suites = unittest.TestSuite(suites)
unittest.TextTestRunner(verbosity=1).run(suites)
