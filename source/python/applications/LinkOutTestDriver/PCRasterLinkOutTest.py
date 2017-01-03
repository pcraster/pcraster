import unittest
from PCRasterLinkOut import c
import ctypes
import os

# Do not print in regression
# testing
_enablePrintInExamples = True
if "PCRTREE2" in os.environ:
  _enablePrintInExamples = False

class PCRasterLinkOutTest(unittest.TestCase):

  def test_c_NonExistant(self):

    script = c.pcr_createScriptFromXMLFile("nonExistant")
    self.failUnless(script != None)
    self.failUnless(not c.pcr_ScriptError(script))

    # error cause non existant
    c.pcr_ScriptExecute(script)
    self.failUnless(c.pcr_ScriptError(script))
    error = c.pcr_ScriptErrorMessage(script)
    self.failUnless(error.find("unable to open primary document entity") >= 0 )

  def testDynamicModel(self):

    nrCells = 2 * 2
    scalarNonSpatialBufferType = ctypes.c_float * 1
    scalarSpatialBufferType = ctypes.c_float * nrCells

    script = c.pcr_createScriptFromXMLFile("dynamicModel.xml")
    self.failUnless(script != None)
    self.failUnless(not c.pcr_ScriptError(script))

    # initialize data array with all 0 ptrs
    dataTransferArraySize = 6
    dataTransferArrayType = ctypes.c_void_p * dataTransferArraySize
    data = dataTransferArrayType()


    # create buffer 0: initialInput: a NonSpatial with value 2.5
    initialInputBuffer     = scalarNonSpatialBufferType( 2.5 )
    # register buffer in data
    data[0] = ctypes.cast(initialInputBuffer,ctypes.c_void_p)

    # create buffer 4: memOutputInitial
    memOutputInitialBuffer = scalarNonSpatialBufferType(-1)
    self.failUnlessEqual(memOutputInitialBuffer[0], -1)
    data[4] = ctypes.cast(memOutputInitialBuffer,ctypes.c_void_p)

    # call initial, data has values for data used in initial
    # other entries are 0
    c.pcr_ScriptExecuteInitialStepMemory(script, data)
    self.failUnless(not c.pcr_ScriptError(script))

    # result of initial is 2 * 2.5
    self.failUnlessEqual(memOutputInitialBuffer[0], 5.0)

    # set up data for dynamic part

    # create buffer 1: dynamicInput, initialize with 1
    dynamicInput = scalarSpatialBufferType(1, 1, 1, 1)
    data[1] = ctypes.cast(dynamicInput,ctypes.c_void_p)

    # buffer 2: the indexedArray for memInputRelation
    # 1 dimension of lenght 2
    class indexedArrayType(ctypes.Structure):
      _fields_ = [ ("nrDims", ctypes.c_uint32),
                   ("lengthOfDimension1", ctypes.c_uint32),
                   ("values", ctypes.c_float * 2) ]

    indexedArray = indexedArrayType()
    indexedArray.nrDims = 1
    indexedArray.lengthOfDimension1 = 2
    indexedArray.values[0] = 4.5 # map lookup value of 1 to 4.5
    indexedArray.values[1] = 0.5 # map lookup value of 1 to 0.5

    data[2] = ctypes.cast(ctypes.addressof(indexedArray),ctypes.c_void_p)

    # create buffer 3: memOutputDynamic, initialize with 1
    memOutputDynamic = scalarSpatialBufferType(1, 1, 1, 1)
    data[3] = ctypes.cast(memOutputDynamic,ctypes.c_void_p)

    # create buffer 5: for rainZone
    nominalSpatialBufferType = ctypes.c_uint32 * nrCells
    rainZoneBuffer = nominalSpatialBufferType(
       2, 2,  # first row will read 2nd memInputRelation array entry
       1, 8   # 2nd row: first col, read 2nd memInputRelation array entry
              #          2nd   col, yield MV since there is no 8th entry in memInputRelation array
             )
    data[5] = ctypes.cast(rainZoneBuffer,ctypes.c_void_p)

    # execute timestep
    step = 1
    while step <= 3:
      status = c.pcr_ScriptExecuteNextTimeStepMemory(script, data)
      self.failUnless(status >= 0)
      if status == 0 and _enablePrintInExamples:
        print("last timestep executed")

      if _enablePrintInExamples:
        print("memOutputDynamic at step %d: (\n%f, %f,\n%f, %f ) \n " % \
                                   (step,
                                    memOutputDynamic[0],
                                    memOutputDynamic[1],
                                    memOutputDynamic[2],
                                    memOutputDynamic[3]))

      # update the dynamicInput
      # multiply the value by 10
      for i in range(nrCells):
        dynamicInput[i] *= 10

      # update the lookup table dynamic
      for i in range(2):
        indexedArray.values[i] -= 0.1

      error = c.pcr_ScriptErrorMessage(script)
      self.failUnless(not c.pcr_ScriptError(script))

      step = step + 1
