#ifndef INCLUDED_OLDCALC_ZEROMAP
#define INCLUDED_OLDCALC_ZEROMAP

#include "stddefx.h"
#include "calc_spatial.h"



namespace calc {
  // ZeroMap declarations.
}



namespace calc {



//! a Spatial that has 0 everywhere
/*!
 *  actually 0 in mask, MV outside.
 *
 *  Tricky stuff, since most of code simply see its base class Spatial.
 *  Note that ZeroMap starts its live as 0, but then d_val can become populated
 *  with data and setCell() may alter its contents.  So ZeroMap is only a trick to store efficiently, until it is needed. A better name would be initiallyZeroMap.
 *  Magic is done by checking the valuePtr().
 */
class ZeroMap : public Spatial
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ZeroMap&           operator=           (const ZeroMap&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ZeroMap               (const ZeroMap&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ZeroMap               (const Spatial *f);

           ~ZeroMap              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void loadExternal()     const override;
  Spatial *copy() const override;

  void   analyzeBoolean(bool& noneAreTrue,bool& noneAreFalse) const override;
  bool   getCell(double& value, size_t i) const override;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
