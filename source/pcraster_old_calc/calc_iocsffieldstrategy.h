#ifndef INCLUDED_OLDCALC_IOCSFFIELDSTRATEGY
#define INCLUDED_OLDCALC_IOCSFFIELDSTRATEGY

#include "stddefx.h"
#include "calc_iofieldstrategy.h"



namespace calc {
  // IoCsfFieldStrategy declarations.
}



namespace calc {



//! Implements the CSF (default PCRaster) input/output strategy
class IoCsfFieldStrategy : public IoFieldStrategy
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  IoCsfFieldStrategy&           operator=           (const IoCsfFieldStrategy&);

  //! Copy constructor. NOT IMPLEMENTED.
                   IoCsfFieldStrategy               (const IoCsfFieldStrategy&);

  //! name of clone detected
  std::string      d_cloneNameCsf;
  //! loc. attributes as taken from d_cloneNameCsf
  geo::RasterSpace d_rasterSpaceCsf;

protected:
  IoFieldStrategy* checkInputMap(VS&   vs, const std::string &fName) override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IoCsfFieldStrategy               ();

  /* virtual */    ~IoCsfFieldStrategy              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void                     checkClone(const std::string& mapFileName) override;

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
