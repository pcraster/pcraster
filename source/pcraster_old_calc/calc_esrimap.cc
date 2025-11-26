#include "stddefx.h"
#include "calc_esrimap.h"
#include "gioapi.h"
#include "appargs.h"
#include "geo_simpleraster.h"
#include "com_csfcell.h"
#include "com_exception.h"
#include "com_new.h"
#include "com_file.h"
#include "calc_esrigridio.h"
#include "calc_vs.h"
#include "calc_map2csf.h" // biggestCellRepr

#include <algorithm>
#include <cmath>

calc::NotAnEsriGrid::NotAnEsriGrid():
 com::Exception(" not an ESRI grid")
{
}

com::PathName calc::EsriMap::prjFilePath() const
{
  com::PathName prjFilePath(fileName());
  prjFilePath+="prj.adf";
  return prjFilePath;
}

//! open map, throws NotAnEsriGrid if not exist or not an Esri Grid
calc::EsriMap::EsriMap(const std::string& fileName):
  calc::GridMap(fileName,0,0,VS_FIELD)
{
/*
 * for (size_t i=0; i < 5; i++)
 *   d_box[i]=0;
 */
  if (!exists(fileName))
    throw NotAnEsriGrid();

  if (com::exists(prjFilePath()))
    d_prjFile = prjFilePath().toString();

  calc::EsriGridIO::bndCellRead(fileName, d_box);
  double min = NAN;
  double max = NAN; // some values not doing bool/ldd
  try {
  calc::EsriGridIO::StaGetMinmaxDbl(fileName, &min, &max);
  } catch (...) {
    min=-1; max=10;
  }
  int    cellType = 0;

  d_chanId = calc::EsriGridIO::cellLayerOpen(
     fileName,READONLY,ROWIO, &cellType,&d_cellSize);
  if (cellType == CELLFLOAT)
    d_vs = VS_S;
  else {
    d_vs = VS_NO;
    if (min >= 0 && max <= 1)
      d_vs = unionSet(d_vs,VS_B);
    if (min >= 1 && max <= 9)
      d_vs = unionSet(d_vs,VS_L);
  }

  // EsriGridIO::privateWindowBox(d_chanId, d_box);
  size_t nrRows = 0;
  size_t nrCols = 0;
  double dummyAdjBndBox[4] = { 0,0,0,0 };
  calc::EsriGridIO::privateAccessWindowSet(d_chanId, d_box, d_cellSize,
    dummyAdjBndBox, nrRows, nrCols);
  d_nrRows = nrRows;
  d_nrCols = nrCols;
  EsriGridIO::privateAccessWindowClear(d_chanId);
}

//! tests if \a gridName is an esrigrid
/*! return true if exists and is an esrigrid, return false otherwise
 */
bool calc::EsriMap::exists(const std::string& gridName)
{
  return calc::EsriGridIO::gridExists(gridName);
}

//! remove an esrimap
/*! if \a gridName exists and is an esriGrid then remove it
    and return true, return false otherwise
 */
bool calc::EsriMap::remove(const std::string& gridName)
{
  if (exists(gridName)) {
    calc::EsriGridIO::gridDelete(gridName);
    return true;
 }
 return false;
}


calc::EsriMap::EsriMap(
    const std::string& fileName,
    size_t nrRows,
    size_t nrCols,
    double cellSize,
    const double box[4],
    VS vs):
  calc::GridMap(fileName,nrRows,nrCols,vs)
{
  d_chanId = calc::EsriGridIO::cellLayerCreate(
    fileName, READWRITE, ROWIO,
    (biggestCellRepr(vs) == CR_REAL4) ? CELLFLOAT : CELLINT,
    cellSize, box);
  d_box[0] = box[0]; d_box[1] = box[1];
  d_box[2] = box[2]; d_box[3] = box[3];
  d_vs=vs;
  d_nrRows = nrRows;
  d_nrCols = nrCols;
  d_cellSize = cellSize;
}

calc::EsriMap::~EsriMap()
{
  close();
  if (d_prjFile.size())
    com::copy(d_prjFile,prjFilePath());
}

void calc::EsriMap::close() const
{
  if(d_chanId >= 0) {
// if (d_vs == VS_B)
//  calc::EsriGridIO::cellLyrCloseNoVat(d_chanId);
// else
    calc::EsriGridIO::cellLyrClose(d_chanId);
  }
  d_chanId=-1;
}

void calc::EsriMap::bbox(double *setThis) const
{
  setThis[0] = d_box[0];
  setThis[1] = d_box[1];
  setThis[2] = d_box[2];
  setThis[3] = d_box[3];
}

bool calc::EsriMap::cmpBox(const double *otherBox)  const
{
 return (otherBox[0] == d_box[0]) &
  (otherBox[1] == d_box[1]) &
  (otherBox[2] == d_box[2]) &
  (otherBox[3] == d_box[3]) ;
}

calc::EsriMap::Window calc::EsriMap::window()
{
  size_t nrRows = 0;
  size_t nrCols = 0;
  double dummyAdjBndBox[4] = { 0,0,0,0 };

  calc::EsriGridIO::privateAccessWindowSet(d_chanId, d_box,
      d_cellSize, dummyAdjBndBox, nrRows, nrCols);

  POSTCOND(d_nrRows == nrRows);
  POSTCOND(d_nrCols == nrCols);
  POSTCOND(cmpBox(dummyAdjBndBox));

  return {d_chanId};
}

calc::EsriMap::Window::Window(int winChan):
  d_winChan(winChan)
{}

calc::EsriMap::Window::~Window()
{
  calc::EsriGridIO::privateAccessWindowClear(d_winChan);
}


void calc::EsriMap::readInBuffer(VS readAs, void *val)
{
  PRECOND(d_chanId >= 0);

  Window const workWindow(window());

  size_t const len = nrCells();
  if (!val) {
   switch(bytesPerCell(vs())) {
    case 1: val = new UINT1[len]; break;
    case 4: val = new  INT4[len]; break;
   }
  }

  switch(biggestCellRepr(readAs)) {
    case CR_REAL4: readFloat(val);
      break;
    case CR_INT4: readInt(val);
      break;
    case CR_UINT1: {
      INT4 *val4 = new INT4[nrCells()];
      readInt(val4);
      switch(readAs) {
        case VS_B: com::copyCells2Boolean((UINT1 *)val, val4, nrCells());
              break;
        case VS_L: com::copyCells((UINT1 *)val, val4, nrCells());
              break;
        default: PRECOND(false);
      }
      delete [] val4;
    }
    break;
    default:
      PRECOND(false);
  }
}

bool calc::EsriMap::getMinMax(double& min, double& max) const
{
  float mvVal = NAN;
  calc::EsriGridIO::getMissingFloat(&mvVal);

  // have to close and re-open again
  close();

  try {
    calc::EsriGridIO::StaGetMinmaxDbl(fileName(), &min, &max);
  } catch (...) {
    return false;
  }

  // re-open again
  int    cellTypeDummy = 0;
  double cellSizeDummy = NAN;
  d_chanId = calc::EsriGridIO::cellLayerOpen(
     fileName(),READONLY,ROWIO, &cellTypeDummy,&cellSizeDummy);

  PRECOND(min != mvVal);
  return true;
}

void calc::EsriMap::writeData(const void *allValues)
{
  PRECOND(d_chanId >= 0);
  Window const workWindow(window());

  switch(biggestCellRepr(vs())) {
  case CR_REAL4: {
    float **outVal2d = com::new2d<float>(nrRows(),nrCols());
    try {
    const auto *inVal = static_cast<const float *>(allValues);
    float *outVal= outVal2d[0];
    float mvVal = NAN;
    calc::EsriGridIO::getMissingFloat(&mvVal);
    size_t const n = nrCells();
    for(size_t i=0; i<n ; i++)
      if (pcr::isMV(inVal[i]))
        outVal[i] = mvVal;
      else {
        outVal[i] = inVal[i];
        if (vs() == VS_D)
          outVal[i] = static_cast<float>(AppOutputDirection(outVal[i]));
      }
    calc::EsriGridIO::putWindowBand(d_chanId,0,nrRows(),
            (const void * const *)outVal2d);
    } catch ( ... ) {
      com::delete2d<float>(outVal2d);
      throw;
    }
    com::delete2d<float>(outVal2d);
    }
    break;
  case CR_INT4: {
    INT4 **outVal2d = com::new2d<INT4>(nrRows(),nrCols());
    const INT4 *inVal= static_cast<const INT4 *>(allValues);
    INT4 *outVal=outVal2d[0];
    for(size_t i=0; i < nrCells(); i++) {
     outVal[i] = inVal[i];
     if (pcr::isMV(inVal[i]))
       outVal[i] = MISSINGINT;
    }
    try {
    calc::EsriGridIO::putWindowBand(d_chanId,0,nrRows(), (const void * const *)outVal2d);
    } catch ( ... ) {
      com::delete2d<INT4>(outVal2d);
      throw;
    }
    com::delete2d<INT4>(outVal2d);
    }
    break;
  case CR_UINT1: {
    INT4 **outVal2d = com::new2d<INT4>(nrRows(),nrCols());
    try {
      const auto *inVal=
        static_cast<const UINT1 *>(allValues);
      INT4 *outVal=outVal2d[0];
      for(size_t i=0; i < nrCells(); i++) {
       if (pcr::isMV(inVal[i]))
         outVal[i] = MISSINGINT;
       else
         outVal[i] = inVal[i];
      }
      calc::EsriGridIO::putWindowBand(d_chanId,0,nrRows(),
          (const void * const *)outVal2d);
    } catch ( ... ) {
      com::delete2d<INT4>(outVal2d);
      throw;
    }
    com::delete2d<INT4>(outVal2d);
    }
    break;
  default:
    PRECOND(false);
  }
}

/*!
 * \todo
 *   in all methods of calc::EsriMap, replace com::new2d calls with
 *   classes that will delete automattically (auto-wise)
 */
void calc::EsriMap::readFloat(void *val)
{
  DEVELOP_PRECOND(val);
  float **ptrVal =
   com::new2d<float>(nrRows(),nrCols(),
                     static_cast<float *>(val));
  float *linVal = ptrVal[0]; // as linear

  calc::EsriGridIO::getWindowBandFloat(
    d_chanId, 0, static_cast<int>(nrRows()), ptrVal);

  // OBSERVATION: this value is the same across
  // grids, despite what you give as the MV value
  // in the input esri grid. GOOD!

  REAL4 mvVal = NAN;
  calc::EsriGridIO::getMissingFloat(&mvVal);
  pcr::AlterToStdMV<REAL4> const ts(mvVal);
  std::for_each(linVal,linVal+nrCells(),ts);

  delete [] ptrVal;
}

void calc::EsriMap::readInt(void *val)
{
  DEVELOP_PRECOND(val);
  PRECOND(sizeof(int) == 4);

  int **ptrVal =
         com::new2d<int>(nrRows(),nrCols(), static_cast<int *>(val));

  calc::EsriGridIO::getWindowBandInt(
    d_chanId, 0, static_cast<int>(nrRows()), ptrVal);

  INT4 *linVal = static_cast<INT4 *>(ptrVal[0]);
  pcr::AlterToStdMV<INT4> const ts(MISSINGINT);
  std::for_each(linVal,linVal+nrCells(),ts);

  delete [] ptrVal;
}

void calc::EsriMap::setPrjFile(const std::string& prjFile)
{
    d_prjFile=prjFile;
}

std::string calc::EsriMap::prjFile() const
{
    return d_prjFile;
}
