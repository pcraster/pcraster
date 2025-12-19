#ifndef INCLUDED_CALC_GLOBAL
#define INCLUDED_CALC_GLOBAL

#include "stddefx.h"
#include "calc_iopimpl.h"
#include "calc_opimplredirect.h"


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

  /* virtual */    ~Global              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             exec                 (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const override;
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

  /* virtual */    ~MRF              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             exec                 (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const override;
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

  /* virtual */   ~OneOfMRF              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             exec                 (RunTimeEnv* rte,
                                         const Operator& op,
                                         size_t nrArgs) const override;
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
