#ifndef INCLUDED_CALC_IFIELDRDCONVERSION
#define INCLUDED_CALC_IFIELDRDCONVERSION

#include "stddefx.h"

#include <cstddef>
#include <limits>



namespace geo {
  class RasterDim;
}



namespace calc {


//! Field to RasterDimension conversion interface
class  IFieldRDConversion {
protected:

                   IFieldRDConversion               () {}
   virtual        ~IFieldRDConversion               () {}
public:
  //! a linear numbering scheme within a (possible) non-rectangular area
  using FieldId = size_t;

  IFieldRDConversion&           operator=           (IFieldRDConversion const& rhs) = default;

                   IFieldRDConversion               (IFieldRDConversion const& rhs) = default;

  //! a linear numbering within the rectangular area
  /*!
      convertible to a (row,col) index if the nrCols of the grid are
      known.
   */
  using RasterId = size_t;


  virtual RasterId            toRasterId(FieldId  fieldId)  const=0;
  //! may return invalidId() if rasterId is not represented in the Field
  virtual FieldId              toFieldId(RasterId rasterId) const=0;

  virtual const geo::RasterDim& rasterDim()                 const=0;
  virtual size_t                nrFieldCells()              const=0;

  //! mark an unused Id
  static size_t  invalidId() {
    return std::numeric_limits<size_t>::max();
  }
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
