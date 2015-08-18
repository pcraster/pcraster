#ifndef INCLUDED_CALC_IOCSFFIELDSTRATEGY
#define INCLUDED_CALC_IOCSFFIELDSTRATEGY



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
  IoFieldStrategy* checkInputMap(VS&   vs, const std::string &fName);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IoCsfFieldStrategy               ();

  /* virtual */    ~IoCsfFieldStrategy              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void                     checkClone(const std::string& mapFileName);

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
