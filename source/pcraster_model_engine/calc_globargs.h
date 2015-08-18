#ifndef INCLUDED_CALC_GLOBARGS
#define INCLUDED_CALC_GLOBARGS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_EXECARGUMENTS
#include "calc_execarguments.h"
#define INCLUDED_CALC_EXECARGUMENTS
#endif



namespace calc {
  // GlobArgs declarations.
  class GlobArg;
  class GlobResult;
}



namespace calc {



//! argument management for the Global functions
class GlobArgs : public ExecArguments
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  GlobArgs&           operator=           (const GlobArgs& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   GlobArgs               (const GlobArgs& rhs);

  void                 **d_voidArgs;
  std::vector<GlobArg *> d_globArgs;

  void                 init(RunTimeEnv *rte);

protected:
  std::vector<GlobResult *>    d_globResults;
  GlobResult *createGlobResult(size_t n);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GlobArgs               (const Operator& op,
                                           RunTimeEnv *rte, size_t nrArgs);

  virtual         ~GlobArgs              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             pushResults            ();
  void        *    dest                   (size_t r=0);
  const void **    src                    ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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


void setMRFResult(RunTimeEnv* rte,size_t result);

} // namespace calc

#endif
