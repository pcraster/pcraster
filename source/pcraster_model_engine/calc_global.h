#ifndef INCLUDED_CALC_GLOBAL
#define INCLUDED_CALC_GLOBAL



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
#ifndef INCLUDED_CALC_OPIMPLREDIRECT
#include "calc_opimplredirect.h"
#define INCLUDED_CALC_OPIMPLREDIRECT
#endif


namespace calc {
  // Global declarations.
}



namespace calc {



//! global functions, with old MAP_ interface
class Global: public OpImplRedirect
{
  typedef int (*F)(void *out, const void **ins);

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Global&           operator=           (const Global& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Global               (const Global& rhs);

  const  F d_f;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Global               (F f);
                   Global               (const IOpImpl* redirect);

  /* virtual */    ~Global              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             exec                 (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};

//! Multiple Result Function: global functions with two results
/*!
 */
class MRF: public OpImplRedirect
{
  typedef int (*F)(void *out0, void *out1, const void **ins);

private:

  //! Assignment operator. NOT IMPLEMENTED.
  MRF&           operator=           (const MRF& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   MRF               (const MRF& rhs);

  const F           d_f;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MRF               (F f);
                   MRF               (const IOpImpl* redirect);

  /* virtual */    ~MRF              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             exec                 (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};

//! Single return interface to MRF type function
class OneOfMRF: public IOpImpl
{
private:

  //! Assignment operator. NOT IMPLEMENTED.
  OneOfMRF&           operator=           (const OneOfMRF& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   OneOfMRF               (const OneOfMRF& rhs);

  const  MRF*      d_mrf;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   OneOfMRF              (const MRF *mrf);

  /* virtual */   ~OneOfMRF              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             exec                 (RunTimeEnv* rte,
                                         const Operator& op,
                                         size_t nrArgs) const;
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
