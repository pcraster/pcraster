import math, unittest
import usecase01
import PCRasterBlock
from pcraster.framework import *



class UseCaseTest(unittest.TestCase):

  def setUp(self):
    pass

  def tearDown(self):
    pass

  def testUseCase01(self):
    nrOfSamples = 1
    nrOfTimeSteps = 10
    model = usecase01.UseCase01()
    model.setQuiet(True)
    dynamicFrw = DynamicFramework(model, nrOfTimeSteps)
    dynamicFrw.run()

    block = model.d_block
    stack = PCRasterBlock.voxelStack(block, 1, 1)

    print PCRasterBlock.nrVoxels(stack), model.nrTimeSteps()
    self.failUnless(PCRasterBlock.nrVoxels(stack) <= model.nrTimeSteps())

    timeStep = model.d_timeStep
    timeStepStack = PCRasterBlock.real4VoxelStackData(timeStep, 1, 1)

    for i in xrange(1, PCRasterBlock.nrVoxels(stack) + 1):
      self.failUnlessEqual(math.fmod(
         PCRasterBlock.real4VoxelValue(timeStepStack, i), 1.0), 0.0)
