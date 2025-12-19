#ifndef INCLUDED_CALC_CELLOP
#define INCLUDED_CALC_CELLOP

#include "stddefx.h"
#include "calc_iopimpl.h"



namespace calc {
  // CellOp declarations.
}



namespace calc {



//! Cell operations that retrieve a cell as non-spatial from a spatial
class CellOp : public IOpImpl
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  CellOp&           operator=           (CellOp const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CellOp               (CellOp const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CellOp               ();

  /* virtual */    ~CellOp              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void getCell(RunTimeEnv *rte, size_t i) const;

};

class CellFocus : public CellOp  {
 public:
   void exec(RunTimeEnv *rte, const Operator &op, size_t nrActualArgs) const override;
};

class LddDownstreamCell : public CellOp  {
 public:
   void exec(RunTimeEnv *rte, const Operator &op, size_t nrActualArgs) const override;
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


extern CellFocus         builtIn__cellfocus;
extern LddDownstreamCell builtIn__ldddownstreamcell;

} // namespace calc

#endif
