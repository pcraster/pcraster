#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#define INCLUDED_CALC_IOFIELDSTRATEGY

#include "stddefx.h"
#include "geo_rasterspace.h"
#include "appargs.h"
#include "calc_types.h"
#include "calc_gridstat.h"

#include <memory>
#include <string>

#ifdef DEBUG_DEVELOP
 #include <set>
#endif


namespace calc {
  class Field;
  class StackInfo;
  class RunDirectory;
  class GridMapOut;
}

namespace dal {
    class RasterDriver;
}

namespace calc {

//! An Input/Output Field Strategy defines how grids are read and written.
/*!
   It must detect the correct formats, and check if all formats are compatible,
   e.g. that the clone have equal nr of rows/cols, cellsize etc.

   It may also implement a in-memory map structure for example in support of OpenMI,
   python runtime, S-plus, whatever

   Clone info is managed by 2 members, d_commonRS and d_cloneNameCommon,
   if d_commonRS.valid() && d_cloneNameCommon.empty() then d_commonRS is set
   by a call to setRasterSpace otherwise by setAndCheckCommon()
*/
class IoFieldStrategy
{

private:
  //! No default ctor
                   IoFieldStrategy               ();

  //! Assignment operator. NOT IMPLEMENTED.
  IoFieldStrategy&           operator=           (const IoFieldStrategy&);

  std::shared_ptr<dal::RasterDriver> d_outDriver;

  //! common attributes of supported formats
  /*! valid() is false if not initialized
   */
  geo::RasterSpace d_commonRS;

  //! name of common clone
  /*!
   * empty if not set
   */
  std::string      d_cloneNameCommon;

  //! name of clone detected, empty if not set
  std::string      d_cloneNameCsf;
  //! loc. attributes as taken from d_cloneNameCsf, !valid() if not set
  geo::RasterSpace d_rasterSpaceCsf;

#ifdef DEBUG_DEVELOP
  //! check data that each external file is only read once in readField()
  mutable std::set<std::string> d_readFiles;
#endif

  void checkCommonCloneEqual(const std::string& mapFileName,
                             const geo::RasterSpace& newMap)  const;

  void throwCloneDiffers(const std::string& map1, const std::string& map2) const;

  std::string pathTimeStep1( const RunDirectory& rd,
                             const std::string&  stackName) const;


  void setAndCheckCommon(const std::string& mapFileName,
                         const geo::RasterSpace& mapRs);



public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IoFieldStrategy               (APP_IO_STRATEGY strategyType);
                   IoFieldStrategy               (const IoFieldStrategy&);

     virtual       ~IoFieldStrategy              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void                 setRasterSpace      (const geo::RasterSpace& rs);

  //! detect format of \a par, check if valid
  /*!
     find format of \a par, compare to be valid
     against clone.
     \returns an ptr to this
   */
  void             checkInputMap (VS&   vs, const std::string &fName);

  //! check if mapFileName is compatible with clone
  /*!
     set clone if none set yet.
     \throws com::Exception if clone differs
   */
  void             checkClone         (const std::string& mapFileName);
  //! return path for stack given \a rd
  std::string      inPathStack        (const RunDirectory& rd,
                                       const std::string& stackName,
                                       size_t nrTimeSteps);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void            readField           (void *dest,
                                       const std::string& mapName,
                                       VS vs) const;
  /*! create stack name
     \param stackName includes directory part
   */
  std::string     makeStackItemName   (const std::string& iname, int   atTimeStep) const;
  //! set all info needed for stack after writing all data
  void            setStackInfo        (const StackInfo& s) const;

  void                  removeOutputObject(const std::string& ) const;
  //! create a new Field from external mapName
  GridStat              writeFieldUnpacked        (const std::string& fileName,
                                                   const Field *f) const;

  //! return raster space location attributes
  const geo::RasterSpace&       rasterSpace        () const;

  void                          validateFileName   (const std::string& fileName) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
