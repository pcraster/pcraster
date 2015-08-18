#include "stddefx.h"

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_CALC_FILEWRITER
#include "calc_filewriter.h"
#define INCLUDED_CALC_FILEWRITER
#endif

#ifndef INCLUDED_CALC_WRITEINFO
#include "calc_writeinfo.h"
#define INCLUDED_CALC_WRITEINFO
#endif
#ifndef INCLUDED_CALC_GRIDMAP
#include "calc_gridmap.h"
#define INCLUDED_CALC_GRIDMAP
#endif
#ifndef INCLUDED_CALC_SUBPARAMETER
#include "calc_subparameter.h"
#define INCLUDED_CALC_SUBPARAMETER
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif
#ifndef INCLUDED_CALC_DECOMPRESSEDDATA
#include "calc_decompresseddata.h"
#define INCLUDED_CALC_DECOMPRESSEDDATA
#endif

#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#include "calc_iofieldstrategy.h"
#define INCLUDED_CALC_IOFIELDSTRATEGY
#endif

#ifndef INCLUDED_CALC_STACKINFO
#include "calc_stackinfo.h"
#define INCLUDED_CALC_STACKINFO
#endif


//! ctor
calc::FileWriter::FileWriter(
  const calc::SubParameter& par,
  size_t index,
  VS     vs):
  d_par(par),d_index(index),d_vs(vs)
{
}

//! do I write in dynamic?
bool calc::FileWriter::reportedInDynamic() const
{
  return d_par.reportedInDynamic();
}

bool  calc::FileWriter::writeThisTimeStep(size_t t) const
{
  return d_par.writeInfo()->writeAtTimestep(t);
}
bool  calc::FileWriter::writeCurrentTimeStep() const
{
  return writeThisTimeStep(currentTimeStep());
}

size_t calc::FileWriter::currentTimeStep() const
{
  return d_par.scriptConst().currentTimeStep();
}
size_t calc::FileWriter::nrTimeSteps() const
{
  return d_par.scriptConst().nrTimeSteps();
}

//! should a tss write at each timestep to disk?
bool calc::FileWriter::writeEachTimeStep() const
{
  return d_par.scriptConst().writeEachTimeStep();
}

//! return name as written
std::string calc::FileWriter::baseName() const
{
  return d_par.scriptConst().outputFilePath(
   d_par.outputFileName(d_index));
}

std::string calc::FileWriter::tssFileName() const
{
  return baseName();
}

void calc::FileWriter::removeTssFile() const
{
  d_par.scriptConst().removeOutputObject(tssFileName());
}

std::string calc::FileWriter::mapFileName(size_t t) const
{
  t = t ? t : currentTimeStep();
  if (d_par.writeInfo()->inDynamic())
    return d_par.scriptConst().ioFieldStrategy().makeStackItemName(baseName(),t);
  return baseName();
}

//! create map for current timestep
calc::GridMap *calc::FileWriter::createMap() const
{
  // symbolType matches vs of field
  return d_par.scriptConst().createMap(mapFileName(),d_par.symbolType());
}

void calc::FileWriter::writeMap(
    double& minUpdate,
    double& maxUpdate,
    const void *data) const
{
  std::auto_ptr<GridMap> gm(createMap());
  DecompressedData dd(d_vs);
  d_par.scriptConst().compressor().decompress(dd,data);
  gm->writeSpatial(dd.decompressed());

  double min,max;
  if (gm->getMinMax(min,max)) {
    if (pcr::isMV(minUpdate)) {
      minUpdate=min;
      maxUpdate=max;
    }
    minUpdate = std::min(minUpdate,min);
    maxUpdate = std::max(maxUpdate,max);
  }
}

void calc::FileWriter::writeNonSpatialToMap(const void *dataValue) const
{
  std::auto_ptr<GridMap> gm(createMap());
  gm->writeNonSpatial(dataValue);
}

//! adjust min max for every map part of the map stack
/*!
    happens at end of execution
    \param min minimum, MV if not set
    \param max maximum, MV if not set, assumed MV if \min is MV
*/
void calc::FileWriter::adjustMinMax(
  const double& min, const double& max) const
{
  if (!reportedInDynamic())
    return;

  StackInfo s;
  s.d_min=s.d_max=0;
  s.d_minMaxSet=!pcr::isMV(min);
  if (s.d_minMaxSet) {
    s.d_min=min;
    s.d_max=max;
  }
  s.d_nrTimeSteps=nrTimeSteps();
  s.d_vs =d_par.symbolType();
  s.d_stackName  = baseName();
  s.d_fileWriter = this;

  d_par.scriptConst().ioFieldStrategy().setStackInfo(s);
}
