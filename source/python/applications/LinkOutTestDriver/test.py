import unittest

import  PCRasterLinkOutTest

suites = []
suites.append(unittest.TestLoader().loadTestsFromTestCase(PCRasterLinkOutTest.PCRasterLinkOutTest))
suites = unittest.TestSuite(suites)
unittest.TextTestRunner(verbosity=1).run(suites)
