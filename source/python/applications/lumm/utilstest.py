import os, unittest
import utils

class UtilsTest(unittest.TestCase):

  def setUp(self):
    # Nothing to do.
    pass

  def tearDown(self):
    # Nothing to do.
    pass

  def testParseOptions(self):
    value = ""
    self.assertEqual(utils.parseOptions(value), [])

    value = "SkipOption"
    self.assertEqual(utils.parseOptions(value), ["SkipOption"])

    value = "Option1 Option2"
    self.assertEqual(utils.parseOptions(value), ["Option1", "Option2"])
