size_t nrRows = 3;
size_t nrCols = 4;
double cellSize = 1.5;
double west = 1.0;
double north = 0.0;

// Discretisation objects.
Raster raster(nrRows, nrCols, cellSize, west, north);
Block block(raster);

// Data objects.
RasterData<REAL4> dem(&raster, 5.0); // Filled with 5.
BlockData<INT4> sediment(&block);
BlockData<REAL4> originalThickness(&block);

// Manipulate data objects.
// -> setMissingValue(dem, 0);
// -> setMissingValue(dem, 3);
// -> setValue(raster, 1, 6.0);
// -> setDefaultValue(sediment, 2);

// Manipulate discretisation objects.
setBaseElevation(block, dem);

void setBaseElevation(
         Block& block,
         RasterData<REAL4> const& elevation)
{
  for(size_t i = 0; i < block.nrCells(); ++i) {
    if(!block.cell(i).isMV()) {
      if(elevation.isMV(i)) {
        block.cell(i).setMV();
      }
      else {
        block.cell(i).setBaseElevation(elevation.cell(i));
      }
    }
  }
}

-> setDefaultValue(sediment, 3);
-> add(block, dem);

-> Block erosion = remove(block, 10.0);

void remove(
         Block& block,
         REAL4 const& thickness)
{
  if(!pcr::isMV(thickness)) {
    for(size_t i = 0; i < block.nrCells(); ++i) {
      if(!block.cell(i).isMV()) {
        block.cell(i).remove(thickness);                                  // <--
      }
    }
  }
}

--> BlockData<UINT1> sand(&block); // Create UINT1 block with missing values <--
--> equals(sediment, 2, sand);     // Fill sand with 0 and 1's.

template<typename T>
void equals(
         BlockData<UINT1>& result,
         BlockData<T> const& arg1,
         T const& arg2)
{
  DEVELOP_PRECOND(
         &result.block() == &arg1.block() ||
         result.block() == arg1.block());
  DEVELOP_PRECOND(!pcr::isMV(arg2));

  for(size_t i = 0; i < result.nrCells(); ++i) {
    equals(result.cell(i), arg1.cell(i), arg2);
  }
}

template<typename T>
void equals(
         std::vector<UINT1>& result,
         std::vector<T> const& arg1,
         T const& arg2)
{
  DEVELOP_PRECOND(result.size() == arg1.size());
  DEVELOP_PRECOND(!pcr::isMV(arg2));

  for(size_t i = 0; i < result.size(); ++i) {
    if(pcr::isMV(arg1[i])) {
      pcr::setMV(result[i]);
    }
    else {
      result[i] = arg1[i] == arg2;
    }
  }
}



-> Block clumps = clump(sediment);

