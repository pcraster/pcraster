#ifndef INCLUDED_CALC_LOOKUP
#define INCLUDED_CALC_LOOKUP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_IOPIMPL
#include "calc_iopimpl.h"
#define INCLUDED_CALC_IOPIMPL
#endif



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

  virtual         ~Lookup               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void exec  (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
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

  virtual         ~LookupLinear               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void exec  (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
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
