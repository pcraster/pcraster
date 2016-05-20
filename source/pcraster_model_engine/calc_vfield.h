#ifndef INCLUDED_CALC_VFIELD
#define INCLUDED_CALC_VFIELD



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_DYNAMIC_BITSET
#include <boost/dynamic_bitset.hpp>
#define INCLUDED_BOOST_DYNAMIC_BITSET
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif



namespace calc {
  class Field;
}



namespace calc {

// field of bits, expected to have same size as VField
typedef boost::dynamic_bitset<> BitField;

//! A value field with compile time type T but runtime spatial/nonspatial
/*!
    Unifies read only runtime access of a ISpatial or NonSpatial.
    This interface can be used in algorithms where it is not known
    until runtime wether a NonSpatial or Spatial is used as input,
    while instantiating the algoritm at compile time for each case (such
    as com::forEachNonMV2 does) will bloath code size exponentially,
    e.g. too many arguments.

    \todo
      Allow copying it to improve performance possibly, in that case make d_owningValueSpatial
      a shared_ptr or check if d_owningValueSpatial is needed, where is it used?
*/

template<typename T> class VField {
private:

  // have d_value and d_spatial upfront, so most accessed does not need an offset

  const T *d_value;
  bool     d_spatial;

  T        d_valueNonSpatial;

  //! the number of valid indices in operator[]
  size_t  d_size;
  //! Field owning d_value and must be deleted in dTor, if !0
  Field  *d_owningValueSpatial;

  void           init                   (const Field& f);

  //! Assignment operator. NOT IMPLEMENTED.
  VField&           operator=           (VField const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   VField               (VField const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VField               (const   T& valueN,
                                         size_t  size);
                   VField               (const   T* valueS,
                                         size_t  size);
                   VField               (Field   *f,
                                         size_t  size);
                   VField               (const Field& f,
                                         size_t size);
                   VField               (const Field& f,
                                         BitField& mvField);
  /* virtual */   ~VField               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const T&         operator[]            (size_t i) const;
  size_t           size                  ()         const;
  bool             spatial               ()         const;

  void             updateMVField         (BitField& mvField) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
const T& VField<T>::operator[](size_t i) const {
   DEVELOP_PRECOND(i<d_size);
   return d_value[d_spatial?i:0];
}

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
