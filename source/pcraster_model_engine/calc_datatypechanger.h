#ifndef INCLUDED_CALC_DATATYPECHANGER
#define INCLUDED_CALC_DATATYPECHANGER

#include "stddefx.h"



namespace calc {
  // DataTypeChanger declarations.
  class DataType;
}



namespace calc {

//! set or restrict DataType's and keep track of nr of changes
/*!
 * In support of BuildTypesVisitor and TopDownExprRestrictor
*/
class DataTypeChanger
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  DataTypeChanger&           operator=           (DataTypeChanger const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DataTypeChanger               (DataTypeChanger const& rhs);

  size_t           d_nrChanges{0};

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataTypeChanger               ();

  /* virtual */    ~DataTypeChanger              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             incr                (size_t nrNewChanges=1);
  void             restrict            (DataType& toUpdate,
                                        const DataType& restrict);
  void             update              (DataType& toUpdate,
                                        const DataType& restrict);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  size_t           nrChanges           () const;

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
