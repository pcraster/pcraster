#ifndef INCLUDED_CALC_IOBANDFIELDSTRATEGY
#define INCLUDED_CALC_IOBANDFIELDSTRATEGY

#include "stddefx.h"
#include "calc_iofieldstrategy.h"



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

  /* virtual */    ~IoBandFieldStrategy              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void                     checkClone(const std::string& mapFileName) override;

  IoFieldStrategy*      checkInputMap(VS& vs, const std::string& fName) override;

  const StackReader* createStackReader(
    const RunDirectory& rd,
      const std::string& stackName) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  APP_IO_STRATEGY strategyType() const override;

  calc::GridMap *createMap(const std::string& fileName, VS vs) const override;

  Spatial* newInputMap(const std::string& mapName,VS vs, const Compressor& c)const override;

  std::string makeStackItemName(const std::string& iname, int   atTimeStep) const override;

  void setStackInfo(const StackInfo& s) const override;



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
