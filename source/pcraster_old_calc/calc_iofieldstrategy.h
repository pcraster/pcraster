#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#define INCLUDED_CALC_IOFIELDSTRATEGY

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif
#ifndef INCLUDED_PCRGENXML_IOSTRATEGY
#include "pcrgenxml_iostrategy.h"
#define INCLUDED_PCRGENXML_IOSTRATEGY
#endif

#ifndef INCLUDED_APPARGS
#include "appargs.h" // APP_IO_STRATEGY
#define INCLUDED_APPARGS
#endif

// Module headers.

#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif



namespace calc {
  class FieldMapInputParameter;
  class ParsPar;
  class GridMap;
  class Spatial;
  struct StackInfo;
  class StackReader;
  class RunDirectory;
  class Compressor;
}

namespace calc {

//! An Input/Output Field Strategy defines how grids are read and written.
/*!
   It must detect the correct formats, and check if all formats are compatible,
   e.g. that the clone have equal nr of rows/cols, cellsize etc.
*/
class IoFieldStrategy
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  IoFieldStrategy&           operator=           (const IoFieldStrategy&);

  //! Copy constructor. NOT IMPLEMENTED.
                   IoFieldStrategy               (const IoFieldStrategy&);

  //! common attributes of supported formats
  geo::RasterSpace d_commonRS;

  //! name of common clone
  std::string      d_cloneNameCommon;

  void checkCommonCloneEqual(const std::string& mapFileName,
                             const geo::RasterSpace& newMap)  const;

protected:

  void throwCloneDiffers(const std::string& map1, const std::string& map2) const;

  //! called by setupClone(), as first action
  /*!
      overwrite if format needs, something specific
   */
  virtual void setupFormatSpecificClone();

  std::string pathTimestep1( const RunDirectory& rd,
                             const std::string&  stackName) const;


  void setAndCheckCommon(const std::string& mapFileName,
                         const geo::RasterSpace& mapRs);


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IoFieldStrategy               ();

     virtual       ~IoFieldStrategy              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! action required to have a good clone
  void setupClone();

  //! check if mapFileName is compatible with clone
  /*!
     set clone if none set yet.
     \throws com::Exception if clone differs
   */
  virtual void checkClone(const std::string& mapFileName)=0;
 
  //! return a format specific reader for a stack
  virtual const StackReader* createStackReader(
      const RunDirectory& rd,
      const std::string& stackName)=0;

  FieldMapInputParameter* createFieldMapInputParameter(
      const calc::ParsPar &par);

  //! detect format of \a par, check if valid
  /*!
     find format of \a par, compare to be valid
     against clone.
     \returns an allocated parameter object.
   */
  virtual IoFieldStrategy* checkInputMap(VS& vs, const std::string&)=0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! the strategy this object implements
  virtual APP_IO_STRATEGY strategyType() const=0;


  virtual void removeOutputObject(const std::string& ) const;

  //! create a new map under the given fileName with type vs
  virtual GridMap *createMap(const std::string& fileName, VS vs) const=0;

  //! create a new input map object for format
  virtual Spatial* newInputMap(const std::string& mapName,VS vs, const Compressor& c)const=0;


  /*! create stack name
     \param stackName includes directory part
   */
  virtual std::string makeStackItemName(
      const std::string& stackName,
      int   atTimeStep) const=0;

  //! set all info needed for stack after writing all data
  virtual void setStackInfo(const StackInfo& s) const=0;

  //! return raster space location attributes
  const geo::RasterSpace& rasterSpace() const
  {
    return d_commonRS;
  }

  virtual void validateFileName(const std::string& fileName) const;

  pcrxml::IoStrategy::EnumType xmlType() const;

  static IoFieldStrategy*  createOnGlobalOption();

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
