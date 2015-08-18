# Testscript to see whether we can convert pcrcalc map to discr::Raster.
from pcraster import *
from PCRasterBlock import *


setclone("dem.map")

# ------------------------------------------------------------------------------
# Convert from discr::RasterData<REAL4> to Spatial.
# This is needed to be able to use PPL functions on PBL results.

# Create a RasterData<REAL4> object.
nrRows = 100
nrCols = 80
cellSize = 10.0
west = 182140
north = 327880
raster = Raster(nrRows, nrCols, cellSize, west, north)
elevation = REAL4RasterData(raster, 5.0)

# Call a PPL function.
field = rasterDataToScalarField(elevation)
result = uniform(defined(field))

# ------------------------------------------------------------------------------
# Convert from Field to discr::RasterData<REAL4>.
# This is needed to be able to use PBL functions on PPL results.

# Create a field.
elevation = readmap("dem.map")

# Call a PBL function.
data = fieldToREAL4RasterData(elevation, raster)
block = createBlock(data)

# And convert back to a field.
surface = surfaceElevation(block)
report(rasterDataToScalarField(surface), "surface.map")

