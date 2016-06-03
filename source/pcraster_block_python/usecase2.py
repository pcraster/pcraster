import math, random, sys
# Order matters: pcraster first, then PCRasterBlock.
from pcraster import *
from PCRasterBlock import *



# d_block=block.block() -> skip, changed interface
# d_block.configure(1,1) -> skip, set from the outside, during save/read
# d_block.setBase(self.ElevationIni) -> skip, constructor argument
# -> 1. Create a block discretisation object.
#    2. Create as many blockData objects you like.

# d_block.add(ifthen(d_clone,scalar(50)),
# -> noCompactionAdd
#    mackeyBridgeAdd
#    deHaanAdd

#   ifthen(d_clone,scalar(10.0)), \

#   ifthen(d_clone,nominal(0))) -> skip, changed interface

# Elevation=d_block.surface()
# -> surfaceElevation(blockData)

# d_block.remove(Erosion) -> skip, changed interface
# -> remove(block, rasterDataWithElevations)

# d_sampleData.d_block.resample(0.4) -> changed interface

# d_sampleData.d_block.saveAsBinary() -> use profile which returns a raster



class DemoModel(Model):
  def __init__(self, argv):
    # Model.__init__(self, argv, nrSamples=25, nrTimeSteps=25, clone="dem.map")
    # Model.__init__(self, argv, nrSamples=1, nrTimeSteps=25, clone="dem.map")
    Model.__init__(self, argv, nrSamples=3, nrTimeSteps=5, clone="dem.map")
    self.setDebug(True)
    self.setTrace(False)
    self.setForkSamples(False)
    self.d_extremeElevationsSet = False
    self.d_minElevation, self.d_maxElevation = 0.0, 0.0

  def premcloop(self):
    self.createRaster()
    self.configureCompactors()

  def stochastic(self):
    self.createBlock()

  def initial(self):
    timeStepDuration = 36000.0
    self.d_sediment = createINT4BlockData(self.d_block, 0)
    self.d_initialThickness = createREAL4BlockData(self.d_block, 0.0)
    self.d_cummulativeLoad = createREAL4BlockData(self.d_block, 0.0)
    self.d_cummulativeDuration = createREAL4BlockData(self.d_block, \
         timeStepDuration)

  def dynamic(self):
    random = uniform(1)
    sedimentToAdd = ifthenelse(random < 0.33, nominal(0), \
         ifthenelse(random < 0.66, nominal(1), nominal(2)))
    thicknessToAdd = scalar(10.0)
    setDefaultValue(self.d_sediment, \
         int4RasterData(sedimentToAdd, self.d_raster))

    deHaanAdd(self.d_block, self.d_sediment, self.d_initialThickness,
         self.d_cummulativeLoad, self.d_cummulativeDuration,
         real4RasterData(thicknessToAdd, self.d_raster),
         self.d_compactors);

    if self.currentTimeStep() % 3 == 0:
      thicknessToRemove = scalar(5.0)
      remove(self.d_block, real4RasterData(thicknessToRemove, self.d_raster))

  def postdynamic(self):
    self.trackExtremeElevations()
    # TODO create pcrdalpy and write
    # write(self.d_block, self.dataSpace(), self.dataSpaceAddress(),
    # "block.pcrblock")
    # write(self.d_sediment, self.dataSpace(),
    # self.dataSpaceAddress(), "sediment.pcrblock")
    # TODO or pimp generateName to use dal::pathFor -> do this first.
    # TODO pcrdalpy
    write(self.d_block, self.generateName("blk.pcr"))
    write(self.d_sediment, self.generateName("sed.pcr"))

    base = scalarField(baseElevation(self.d_block))
    surface = scalarField(surfaceElevation(self.d_block))
    report(base, self.generateName("base"))
    report(surface, self.generateName("surface"))

  def postmcloop(self):
    # Create a regular block based on the extreme elevations tracked.
    baseElevation = real4RasterData(scalar(self.minElevation()), self.d_raster)
    regularBlock = createBlock(baseElevation)
    thickness = self.maxElevation() - self.minElevation()
    voxelThickness = 0.5
    nrLevels = int(math.ceil(thickness / voxelThickness))

    # noCompactionAdd(regularBlock, nrLevels, voxelThickness)

    thicknesses = real4RasterData(scalar(0.5), self.d_raster)
    for level in range(nrLevels):
      noCompactionAdd(regularBlock, thicknesses)

    frequencies = createREAL4BlockData(regularBlock, 0.0)

    for sample in self.sampleNumbers():
      blockName = generateNameS("blk.pcr", sample)
      sedimentName = generateNameS("sed.pcr", sample)
      block = readBlock(blockName)
      sediment = readINT4BlockData(sedimentName, block)

      regularSediment = resample(sediment, regularBlock)

      sand = equals(regularSediment, 0)
      frequencies = add(frequencies, real4BlockData(sand))

      for i in range(nrLevels):
        level = profile(sediment, self.minElevation() + i * voxelThickness)
        writeBinary(level, generateNameS("level.%d" % (i + 1), sample))
        level = nominalField(level)
        report(level, generateNameST("level", sample, i + 1))

    chances = divide(frequencies, self.nrSamples())
    writeVTK(chances, self.generateName("chances.vtk"))
    writeGSLIB(chances, self.generateName("chances.gslib"))

  def createRaster(self):
    self.d_raster = createRaster(clone())

  def createBlock(self):
    dem = "dem.map"
    dem += 10.0 * uniform(defined(dem))
    baseElevation = real4RasterData(dem, self.d_raster)
    self.d_block = createBlock(baseElevation)

  def configureCompactors(self):
    sandId = 0
    clayId = 1
    peatId = 2
    self.d_sedimentIds = [ sandId, clayId, peatId ]
    b        = { sandId: 0.2 , clayId:  0.3,  peatId: 0.4  }
    c        = { sandId: 0.02, clayId:  0.04, peatId: 0.06 }
    buoyancy = { sandId: 6   , clayId: 10,    peatId: 2    }

    self.d_compactors = DeHaanCompactors()

    for id in self.d_sedimentIds:
      compactor = DeHaanCompactor(b[id], c[id], buoyancy[id])
      self.d_compactors.setCompactor(id, compactor)

  def minElevation(self):
    tuple = cellvalue(self.d_minElevation, 1)
    assert tuple[1] == True
    return tuple[0]

  def maxElevation(self):
    tuple = cellvalue(self.d_maxElevation, 1)
    assert tuple[1] == True
    return tuple[0]

  def trackExtremeElevations(self):
    base = scalarField(baseElevation(self.d_block))
    surface = scalarField(surfaceElevation(self.d_block))

    minElevation = mapminimum(base)
    maxElevation = mapmaximum(surface)

    if not self.d_extremeElevationsSet:
      self.d_minElevation = minElevation
      self.d_maxElevation = maxElevation
      self.d_extremeElevationsSet = True
    else:
      import operations
      self.d_minElevation = operations.min(minElevation, self.d_minElevation)
      self.d_maxElevation = operations.max(maxElevation, self.d_maxElevation)

sys.exit(DemoModel(sys.argv).run(True))
