#ifndef INCLUDED_CALC_EXECARGUMENTS
#define INCLUDED_CALC_EXECARGUMENTS

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
// PCRaster library headers.

// Module headers.



namespace calc {
  // ExecArguments declarations.
  class Field;
  class RunTimeEnv;
  class Operator;
  class DataType;
  class DataValue;
}



namespace calc {

//! manage arguments for a large number of operations ensure clean up, etc.
/*!
 * This helper class supports argument management applicable for a large number
 * of cases but not all. E.g. StatTable does its own management.
 * 2 main interfaces:
 * <ul>
 *  <li> Field's createResult(), operator[]</li>
 *  <li> data-arrays (Field::src(),Field::dest()): dest(),srcDest(),src()</li>
 * </ul>
 *
 */
class ExecArguments
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ExecArguments&           operator=           (const ExecArguments& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ExecArguments               (const ExecArguments& rhs);

  void   clean();

protected:
  const  Operator&     d_op;
  RunTimeEnv          *d_rte;
  std::vector<Field *> d_fields;
  std::vector<Field *> d_result;

  // d_readOnlyReferenceBug hack
  std::set<Field*>     d_doNotDelete;

  //! index in d_fields in case d_result is a src resuse, or >= d_fields.size() otherwise
  size_t               d_resultIsField;
  DataValue           *d_firstNonFieldInput;

  DataType             resultType(size_t r) const;

  void                 pushResult (Field *result);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ExecArguments              (const Operator& op,
                                               RunTimeEnv *rte,
                                               size_t nrFieldInputs);

  virtual         ~ExecArguments              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  Field&           createResult               ();
  void             createResults              ();

  void            *dest                       (size_t d=0);
  void            *srcDest                    (size_t a);

  virtual void     pushResults                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const void      *src                        (size_t a) const;

  const Field&     operator[]                 (size_t a) const;
  size_t           size                       () const;
  Field&           result                     (size_t r=0) const;
  DataValue*       firstNonFieldInput         () const;
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
