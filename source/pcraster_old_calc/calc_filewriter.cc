#include "stddefx.h"
#include "com_csfcell.h"
#include "calc_filewriter.h"
#include "calc_writeinfo.h"
#include "calc_gridmap.h"
#include "calc_subparameter.h"
#include "calc_iscript.h"
#include "calc_compressor.h"
#include "calc_decompresseddata.h"
#include "calc_iofieldstrategy.h"
#include "calc_stackinfo.h"

#include <algorithm>
#include <cmath>
#include <memory>

//! ctor
calc::FileWriter::FileWriter(const calc::SubParameter &par, size_t index, VS vs)
    : d_par(par), d_index(index), d_vs(vs)
{
}

//! do I write in dynamic?
bool calc::FileWriter::reportedInDynamic() const
{
  return d_par.reportedInDynamic();
}

bool calc::FileWriter::writeThisTimeStep(size_t t) const
{
  return d_par.writeInfo()->writeAtTimestep(t);
}

bool calc::FileWriter::writeCurrentTimeStep() const
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
  return d_par.scriptConst().outputFilePath(d_par.outputFileName(d_index));
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
  if (d_par.writeInfo()->inDynamic()) {
    return d_par.scriptConst().ioFieldStrategy().makeStackItemName(baseName(), t);
  }
  return baseName();
}

//! create map for current timestep
calc::GridMap *calc::FileWriter::createMap() const
{
  // symbolType matches vs of field
  return d_par.scriptConst().createMap(mapFileName(), d_par.symbolType());
}

void calc::FileWriter::writeMap(double &minUpdate, double &maxUpdate, const void *data) const
{
  std::unique_ptr<GridMap> gm(createMap());
  DecompressedData dd(d_vs);
  d_par.scriptConst().compressor().decompress(dd, data);
  gm->writeSpatial(dd.decompressed());

  double min = NAN;
  double max = NAN;
  if (gm->getMinMax(min, max)) {
    if (pcr::isMV(minUpdate)) {
      minUpdate = min;
      maxUpdate = max;
    }
    minUpdate = std::min(minUpdate, min);
    maxUpdate = std::max(maxUpdate, max);
  }
}

void calc::FileWriter::writeNonSpatialToMap(const void *dataValue) const
{
  std::unique_ptr<GridMap> gm(createMap());
  gm->writeNonSpatial(dataValue);
}

//! adjust min max for every map part of the map stack
/*!
    happens at end of execution
    \param min minimum, MV if not set
    \param max maximum, MV if not set, assumed MV if \min is MV
*/
void calc::FileWriter::adjustMinMax(const double &min, const double &max) const
{
  if (!reportedInDynamic()) {
    return;
  }

  StackInfo s;
  s.d_min = s.d_max = 0;
  s.d_minMaxSet = !pcr::isMV(min);
  if (s.d_minMaxSet) {
    s.d_min = min;
    s.d_max = max;
  }
  s.d_nrTimeSteps = nrTimeSteps();
  s.d_vs = d_par.symbolType();
  s.d_stackName = baseName();
  s.d_fileWriter = this;

  d_par.scriptConst().ioFieldStrategy().setStackInfo(s);
}
