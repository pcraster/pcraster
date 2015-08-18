from pcraster import *
from PCRasterBlock import *



class UseCase01(Model):

  def __init__(self, argv):
    Model.__init__(self, argv, nrSamples=1, nrTimeSteps=10, clone="clone.map")

  def initial(self):
    self.d_raster = createRaster(clone())
    baseElevation = real4RasterData(uniform(1), self.d_raster)
    self.d_block = createBlock(baseElevation)
    self.d_timeStep = createREAL4BlockData(self.d_block, 0)

    setDefaultValue(self.d_timeStep,
         real4RasterData(scalar(0), self.d_raster))
    noCompactionAdd(self.d_block, real4RasterData(10 * uniform(1), self.d_raster))

  def dynamic(self):
    remove(self.d_block, real4RasterData(uniform(1), self.d_raster))
    setDefaultValue(self.d_timeStep,
         real4RasterData(scalar(self.currentTimeStep()), self.d_raster))
    noCompactionAdd(self.d_block, real4RasterData(10 * uniform(1), self.d_raster))

  def postdynamic(self):
    pass
    # write(self.d_block, self.generateName("block.pcr"))
    # write(self.d_timeStep, self.generateName("timestep.pcr"))

  def postmcloop(self):
    pass
