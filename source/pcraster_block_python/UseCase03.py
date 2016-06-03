from pcraster import *
from PCRasterBlock import *

# Required for PCRaster extension. Use the name of an existing raster.
setclone("dem")

# Configure new block.
raster = createRaster(clone())
baseElevation = real4RasterData(uniform(1), raster)
block = createBlock(baseElevation)

# Create attributes, link to block.
presence = createUINT1BlockData(block, 1)
sediment = createINT4BlockData(block, 5)
concentration = createREAL4BlockData(block, 0.0)

for i in range(1, 11):
  # Set default value for new cells for each attribute.
  setDefaultValue(presence, uint1RasterData(boolean(i % 2), raster))
  setDefaultValue(sediment, int4RasterData(nominal(i), raster))
  setDefaultValue(concentration, real4RasterData(scalar(i), raster))

  # Extent block, which will add attribute values also.
  noCompactionAdd(block, real4RasterData(10 * uniform(1), raster))

# Get discretisation and data of north-west cell.
stack = voxelStack(block, 1, 1)
presenceStack = uint1VoxelStackData(presence, 1, 1)
concentrationStack = real4VoxelStackData(concentration, 1, 1)
sedimentStack = int4VoxelStackData(sediment, 1, 1)

# Calculate profile raster.
presenceLevel = profile(presence, 3.0)
sedimentLevel = profile(sediment, 3.0)
concentrationLevel = profile(concentration, 3.0)
report(scalarField(concentrationLevel), "concentrationLevel-3.0")

# Write block discretisation and attributes.
write(block, "block.pcrblock")
write(presence, "presence.pcrblock")
write(sediment, "sediment.pcrblock")
write(concentration, "concentration.pcrblock")

del presence
del sediment
del concentration
del block

# Read block discretisation and attributes.
block = readBlock("block.pcrblock")
presence = readUINT1BlockData("presence.pcrblock", block)
sediment = readINT4BlockData("sediment.pcrblock", block)
concentration = readREAL4BlockData("concentration.pcrblock", block)



