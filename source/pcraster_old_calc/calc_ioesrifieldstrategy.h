#ifndef INCLUDED_CALC_IOESRIFIELDSTRATEGY
#define INCLUDED_CALC_IOESRIFIELDSTRATEGY



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.

#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#include "calc_iofieldstrategy.h"
#define INCLUDED_CALC_IOFIELDSTRATEGY
#endif



namespace calc {
  class EsriGridIO;
}

namespace calc {

//! Defined input/output strategy if Esri Grids is the preferred format
/*
   key issues:
   <ol>
   <li>to set the RasterSpace features from an ESRI input
   grid if that is avaialable. Only if there is no ESRI format input then
   one can set the features from another (CSF) format. This is to
   the changes that ESRI grids input and output do not match on the features
   exactly.
   <li>
      If a file is not recognized as Esri Grid, it tries to read it as
      a PCRaster csf file. If there is both Esri and Csf input maps then
      then both clone must be in sync.
   </ol>
 */
class IoEsriFieldStrategy : public IoFieldStrategy
{
private:

  //! loading of GridIO-dll
  EsriGridIO         *d_esriGrid;

  //! fall back strategy
  /*!
      if something is not recognized as Esri then
      we use the CSf strategy
   */
  IoFieldStrategy *d_fallBack;

  //! the bounding box args needed in Grid API
  double d_bbox[4];

  //! name of clone that is an Esri grid
  /*! can be empty if none recognized as Esri
   */
  std::string      d_cloneNameEsri;

  //! empty if not set, otherwise an existing .prj file
  std::string      d_prjFile;

  /*! loc. attributes found from Esri grid
      if not set when non found (d_cloneNameEsri empty)
      then set in calc::IoEsriFieldStrategy::setupFormatSpecificClone()
      to CSF clone
   */
  geo::RasterSpace d_rasterSpaceEsri;

  //! Assignment operator. NOT IMPLEMENTED.
  IoEsriFieldStrategy&           operator=           (const IoEsriFieldStrategy&);

  //! Copy constructor. NOT IMPLEMENTED.
                   IoEsriFieldStrategy               (const IoEsriFieldStrategy&);

protected:
   IoFieldStrategy* checkInputMap(VS &vs, const std::string &fName);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IoEsriFieldStrategy               ();

  /* virtual */    ~IoEsriFieldStrategy              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void                     checkClone(const std::string& mapFileName);

  void setupFormatSpecificClone();

  const StackReader* createStackReader(
    const RunDirectory& rd,
    const std::string& stackName);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  APP_IO_STRATEGY strategyType() const;

  GridMap *createMap(const std::string& fileName, VS vs) const;

  Spatial* newInputMap(const std::string& mapName,VS vs, const Compressor& c)const;

  void removeOutputObject(const std::string& objName) const;

  std::string makeStackItemName(const std::string& iname, int   atTimeStep) const;

  void setStackInfo(const StackInfo& s) const;


  void validateFileName(const std::string& fileName) const;

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
