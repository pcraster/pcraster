import os, os.path
import unittest
import pcraster._pcraster as _pcraster
import pcraster

class TestCase(unittest.TestCase):

  def setUp(self):
    _pcraster._initGlobals()
    pcraster.setrandomseed(1)

  def tearDown(self):
    pass

  # def readInputField(self, filename):
  #   return readmap(filename)

  def readValidatedField(self, filename):
    return pcraster.readmap(os.path.join("validated", filename))

  def mapEquals(self, result, filename):

    import math
    def floatEquals(float1, float2):
      threshold = 1e-6
      return math.fabs(float1 - float2) <= threshold

    validatedMap = self.readValidatedField(filename)

    if((result.dataType() == _pcraster.Scalar) | \
         (result.dataType() == _pcraster.Directional)):
      result = _pcraster._closeAtTolerance(result, validatedMap)
    else:
      result = pcraster.pcreq(result, validatedMap)


    min = pcraster.mapminimum(pcraster.scalar(result))
    value, isValid = pcraster.cellvalue(min, 1)
    assert isValid
    return floatEquals(value, 1.0)
