#ifndef INCLUDED_CALC_FILEWRITER
#define INCLUDED_CALC_FILEWRITER


#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

namespace geo {
class RasterSpace;
}

namespace com {
class PathName;
}

namespace calc {

class SubParameter;
class GridMap;

//! Controls output to (a) file(s)
/*!
   A FileWriter object is created for each parameter array element.
 */
class FileWriter {
private:
  //! the parameter written
  const SubParameter& d_par;
  //! the array element written
  size_t              d_index;
  //! vs of the element written
  VS                  d_vs;

  bool reportedInDynamic() const;

  std::string baseName() const;

  GridMap *createMap() const;

public:
  FileWriter(const SubParameter& par, size_t index, VS vs);

  //! is current timestep written
  bool  writeCurrentTimeStep() const;

  //! is timestep t written
  bool  writeThisTimeStep(size_t t) const;

  //! the name for a map, if t <> 0 then fetch a stack name
  std::string  mapFileName(size_t t=0) const;

  //! the name for a timeseries
  std::string  tssFileName() const;

  //! remove this
  void removeTssFile() const;

  //! the current timestep
  size_t       currentTimeStep() const;

  //! number of timesteps
  size_t       nrTimeSteps() const;

  bool         writeEachTimeStep() const;


  void writeMap(double& minUpdate, double& maxUpdate, const void *data) const;
  void writeNonSpatialToMap(const void *dataValue) const;

  void adjustMinMax(const double& min, const double& max) const;
};

}

#endif
