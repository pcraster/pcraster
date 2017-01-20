#!/usr/bin/env python2.4
from pcraster import *
from PCRasterBlock import *

nrRows =  3
nrCols =  2
cellSize = 1
west = 0.0
north = 0.0

# Use pcrasterpy for all rasters.
raster = Raster(nrRows, nrCols, cellSize, west, north)
baseElevation = REAL4RasterData(raster, 0.0)
block = createBlock(baseElevation)

originalThickness = REAL4BlockData(block, 0.0) # Not used.
thickness = REAL4RasterData(raster, 1)
maxVoxelThickness = 0.1

compactors = Compactors()
age = REAL4BlockData(block, 12.5)
sediment = INT4BlockData(block, 3)
# sediment = INT4BlockData(block, "aboolean.pcr")
# ? data type sediment??


for i in range(block.nrCells()):
  print(i)
  sediment.setDefaultValue(i)
  compactors.setCompactor(i, DummyCompactor())
  setMVREAL4(thickness, i)
  # TODO block kan eruit.

  mackeyBridgeAdd(block, originalThickness, sediment, thickness,
         maxVoxelThickness, compactors)
  # thickness="dep.map"
  # sedimentation="sed.map"
  # add(block, thickness, maxVoxelThickness, compactors,
  #        sediment = sedimentation, age="age.map")
  # add("sed.map", "age.map", thickness, maxVoxelThickness, compactors)
  # add("age.map", "sed.map", thickness, maxVoxelThickness, compactors)
  #
  #sediment.default("sed.map")
  #age.default("age.map")
  # add(block, thickness, maxVoxelThickness, compactors)

  # sand = sediment == 3
  # Ksat=if(sand,12.4,4.0)
  # Q=modflow(block)

regularBlock = resampleBlock(block, 0.2)
# writeREAL4BlockDataVTK(
#          resampleREAL4BlockData(sediment, regularBlock), "sediment.xml")
writeINT4BlockDataVTK(
         resampleINT4BlockData(sediment, regularBlock), "sediment.xml")

# lowestElevation = mapminimum(baseElevation(block))
lowestElevation = 0.0
# highestElevation = mapminimum(surfaceElevation(block))
# heightDifference = highestElevation - lowestElevation
heightDifference = 100
interval = 50
nrLayers = int(heightDifference / interval)
height = lowestElevation

for i in range(nrLayers):
  profile = profileINT4(sediment, height)
  height += interval
  # report(height, "height%d" % (i))



# Check discretisation.
# for i in range(block.nrCells()):
#   assert len(block.cell(i)) == 16
#   assert block.cell(i).thickness() == 15.5
#   assert block.cell(i)[len(block.cell(i)) - 1] == 0.5

# Check data.
# TODO copy from C++ tests

# elevation = surfaceElevation(block)

# writeBlock(block, "area1.pcrblock")
# writeINT4BlockData(sediment, "sediment.pcrblock")
# sediment2 = readINT4BlockData("sediment.pcrblock", block)

# assert sediment == sediment2

# d_block=block.block() -> skip, changed interface
# d_block.configure(1,1) -> skip, set from the outside, during save/read
# d_block.setBase(self.ElevationIni) -> skip, constructor argument
# d_block.add(ifthen(d_clone,scalar(50)),
#   ifthen(d_clone,scalar(10.0)), \
#   ifthen(d_clone,nominal(0))) -> skip, changed interface
# Elevation=d_block.surface()
# d_block.remove(Erosion) -> skip, changed interface
# d_sampleData.d_block.resample(0.4) -> changed interface
# d_sampleData.d_block.saveAsBinary() -> use profile which returns a raster

print("done")
