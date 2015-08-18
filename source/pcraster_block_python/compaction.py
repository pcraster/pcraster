import random
from PCRasterBlock import *
# from pcraster import *

# ------------------------------------------------------------------------------
# Compaction according to De Haan.
# Change in thickness of a layer.
# compaction = initialThickness -
#        initialThickness * (cummulativeLoad / sigma_po)^-b * time^-c
# ? effectieve belasting / initiele belasting -> cummulativeLoad
# ? cummulativeLoad - cummulativeLoad(t0) == cummulativeLoad ->
#        cummulativeLoad(t0) altijd 0
# ? t / t0 == t / 1 == t -> t altijd op 1

# Cummulative load, including current layer.
# cummulativeLoad = cummulativeLoadUpperLayer +
#        0.5 * initialThickness * buoyancy
# ? 0.5 om de load in het centrum van de voxel te bepalen -> raken we hier niet
#   per cel een halve voxel kwijt?

# Specific gravity depends on material.

# Amount of time units passed.
# time = currentTimeStep * timeStepDuration
# ------------------------------------------------------------------------------

# Configure compaction functions for each sediment.
sandId = 0
clayId = 1
peatId = 2
sedimentIds = [ sandId, clayId, peatId ]
b        = { sandId: 0.2 , clayId:  0.3,  peatId: 0.4  }
c        = { sandId: 0.02, clayId:  0.04, peatId: 0.06 }
buoyancy = { sandId: 6   , clayId: 10,    peatId: 2    }

compactors = DeHaanCompactors()

for id in sedimentIds:
  compactor = DeHaanCompactor(b[id], c[id], buoyancy[id])
  compactors.setCompactor(id, compactor)

# Configure data space.
nrRows = 10
nrCols = 20
cellSize = 1.0
west = 0.0
north = 0.0

nrTimeSteps = 22
timeStepDuration = 36000.0

raster = Raster(nrRows, nrCols, cellSize, west, north)
baseElevation = REAL4RasterData(raster, 0.0)
block = Block(raster, baseElevation)

sediment = INT4BlockData(block, 0)
initialThickness = REAL4BlockData(block, 0.0)
cummulativeLoad = REAL4BlockData(block, 0.0)
cummulativeDuration = REAL4BlockData(block, timeStepDuration)

# Add sediment to study area.
for step in range(nrTimeSteps):
  # thicknessToAdd = uniform(boolean(1))
  thicknessToAdd = random.uniform(0.1, 5.0)
  sedimentToAdd = random.choice(sedimentIds)
  thickness = REAL4RasterData(raster, thicknessToAdd)
  sediment.setDefaultValue(sedimentToAdd)
  deHaanAdd(block, sediment, initialThickness, cummulativeLoad,
         cummulativeDuration, thickness, compactors);

# Save the results.
regularBlock = resampleBlock(block, 0.1)
writeINT4BlockDataVTK(resampleINT4BlockData(sediment, regularBlock),
         "sediment.xml")

