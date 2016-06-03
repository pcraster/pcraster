import math
from pcraster import *
from PCRasterBlock import *



setclone("clone.map")

def createABlock():
  raster = createRaster(clone())
  base = real4RasterData(uniform(1), raster)

  block = createBlock(base)
  sediment = createINT4BlockData(block, 0)

  setDefaultValue(sediment, int4RasterData(nominal(0), raster))
  noCompactionAdd(block, real4RasterData(10 * uniform(1), raster))

  setDefaultValue(sediment, int4RasterData(nominal(1), raster))
  noCompactionAdd(block, real4RasterData(5 * uniform(1), raster))

  setDefaultValue(sediment, int4RasterData(nominal(2), raster))
  noCompactionAdd(block, real4RasterData(6 * uniform(1), raster))

  # Create a regular block based on the extreme elevations tracked.
  base = baseElevation(block)
  minElevation = cellvalue(mapminimum(scalarField(base)), 1)[0]
  maxElevation = cellvalue(mapmaximum(scalarField(surfaceElevation(block))), 1)[0]
  thickness = maxElevation - minElevation
  voxelThickness = 0.5
  nrLevels = int(math.ceil(thickness / voxelThickness))

  regularBlock = createBlock(real4RasterData(scalar(minElevation), raster))
  thicknesses = real4RasterData(scalar(voxelThickness), raster)
  for level in range(nrLevels):
    noCompactionAdd(regularBlock, thicknesses)

  regularSediment = resample(sediment, regularBlock)
  writeVTK(regularSediment, "sediment.vtk")

createABlock()

# class UseCase01(Model):
# 
#   def __init__(self, argv):
#     Model.__init__(self, argv, nrSamples=1, nrTimeSteps=10,
#          clone="../pcrasterpy/clone.map")
# 
#   def initial(self):
#     self.d_raster = createRaster(clone())
#     baseElevation = real4RasterData(uniform(1), self.d_raster)
#     self.d_block = createBlock(baseElevation)
#     self.d_timeStep = createREAL4BlockData(self.d_block, 0)
# 
#     setDefaultValue(self.d_timeStep,
#          real4RasterData(scalar(0), self.d_raster))
#     noCompactionAdd(self.d_block, real4RasterData(10 * uniform(1), self.d_raster))
# 
#   def dynamic(self):
#     remove(self.d_block, real4RasterData(uniform(1), self.d_raster))
#     setDefaultValue(self.d_timeStep,
#          real4RasterData(scalar(self.currentTimeStep()), self.d_raster))
#     noCompactionAdd(self.d_block, real4RasterData(10 * uniform(1), self.d_raster))
# 
#   def postdynamic(self):
#     pass
#     # write(self.d_block, self.generateName("block.pcr"))
#     # write(self.d_timeStep, self.generateName("timestep.pcr"))
# 
#   def postmcloop(self):
#     pass
