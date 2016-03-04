# -*- coding: utf-8 -*-
import os.path
import testcase
import pcraster
from pcraster.framework.aggregationfunctions import mcpercentiles



class PercentileTests(testcase.TestCase):

  def setUp(self):
    testcase.TestCase.setUp(self)
    pcraster.setclone("clone-2x3.map")

  def test001(self):
    """Test mcpercentiles"""
    percentileLevels = [0.01, 0.5, 0.99]
    realisationNumbers = range(1, 100)
    timesteps = [0]
    names = ["concentration_without_mv", "concentration_with_mv"]

    percentiles = mcpercentiles(names, percentileLevels, realisationNumbers,
      timesteps)

    for level in percentileLevels:
      name = "concentration_without_mv_{0}.map".format(level)
      self.assertTrue(os.path.isfile(name))
      raster = pcraster.readmap(name)
      for i in range(1, 7):
        value, valid = pcraster.cellvalue(raster, i)
        self.assertTrue(valid)
        self.assertAlmostEqual(value, i +
          (level * (len(realisationNumbers) - 1)), 5)

      name = "concentration_with_mv_{0}.map".format(level)
      self.assertTrue(os.path.isfile(name))
      raster = pcraster.readmap(name)
      for i in range(1, 7):
        value, valid = pcraster.cellvalue(raster, i)
        self.assertTrue(valid)

        if level < 0.50:
          self.assertAlmostEqual(value, i +
            (level * (len(realisationNumbers)- 1 - 1)), 5)
        elif level > 0.50:
          self.assertAlmostEqual(value, i +
            (level * (len(realisationNumbers)- 1 - 1)) + 1, 5)
        else:
          self.assertAlmostEqual(value, i +
            (level * (len(realisationNumbers)- 1 - 1)) + 0.5, 5)

