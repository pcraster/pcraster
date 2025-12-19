#ifndef INCLUDED_CALC_LOOKUP
#define INCLUDED_CALC_LOOKUP

#include "stddefx.h"
#include "calc_iopimpl.h"



namespace calc {
  // Lookup declarations.
}



namespace calc {


//! Operation Implementation
class Lookup: public IOpImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Lookup&           operator=           (const Lookup& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Lookup               (const Lookup& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Lookup               ();

          ~Lookup               () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void exec  (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const override;
};

//! Operation Implementation
class LookupLinear: public IOpImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  LookupLinear&           operator=           (const LookupLinear& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   LookupLinear               (const LookupLinear& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LookupLinear               ();

          ~LookupLinear               () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void exec  (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const override;
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

extern Lookup       builtIn_lookupnominal;
extern Lookup       builtIn_lookupboolean;
extern Lookup       builtIn_lookupordinal;
extern Lookup       builtIn_lookupscalar;
extern Lookup       builtIn_lookupdirectional;
extern Lookup       builtIn_lookupldd;
extern LookupLinear builtIn_lookuplinear;


} // namespace calc

#endif
