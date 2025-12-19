#ifndef INCLUDED_CALC_GLOBARGS
#define INCLUDED_CALC_GLOBARGS

#include "stddefx.h"
#include "calc_execarguments.h"

#include <vector>


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

  void                 **d_voidArgs{};
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

          ~GlobArgs              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             pushResults            () override;
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
