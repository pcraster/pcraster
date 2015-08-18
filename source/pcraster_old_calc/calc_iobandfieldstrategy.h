#ifndef INCLUDED_CALC_IOBANDFIELDSTRATEGY
#define INCLUDED_CALC_IOBANDFIELDSTRATEGY



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
  // IoBandFieldStrategy declarations.
}



namespace calc {



//! Implements the BandMap input/output strategy used for the WL project
/*!
 *  all the dynamic stack stuff will fail, we do not implement dynamic
 *  data yet.
 */
class IoBandFieldStrategy : public IoFieldStrategy
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  IoBandFieldStrategy&           operator=           (const IoBandFieldStrategy&);

  //! Copy constructor. NOT IMPLEMENTED.
                   IoBandFieldStrategy               (const IoBandFieldStrategy&);

  //! name of clone detected
  std::string      d_cloneNameBand;
  //! loc. attributes as taken from d_cloneNameBand
  geo::RasterSpace d_rasterSpaceBand;


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IoBandFieldStrategy               ();

  /* virtual */    ~IoBandFieldStrategy              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void                     checkClone(const std::string& mapFileName);

  IoFieldStrategy*      checkInputMap(VS& vs, const std::string& fName);

  const StackReader* createStackReader(
    const RunDirectory& rd,
      const std::string& stackName);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  APP_IO_STRATEGY strategyType() const;

  calc::GridMap *createMap(const std::string& fileName, VS vs) const;

  Spatial* newInputMap(const std::string& mapName,VS vs, const Compressor& c)const;

  std::string makeStackItemName(const std::string& iname, int   atTimeStep) const;

  void setStackInfo(const StackInfo& s) const;



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
