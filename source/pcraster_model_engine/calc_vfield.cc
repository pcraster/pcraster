#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_VFIELD
#include "calc_vfield.h"
#define INCLUDED_CALC_VFIELD
#endif

// Library headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h" // updateMVField only
#define INCLUDED_COM_CSFCELL
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif



/*!
  \file
  This file contains the implementation of the VField class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class VFieldPrivate
{
public:

  VFieldPrivate()
  {
  }

  ~VFieldPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC VFIELD MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VFIELD MEMBERS
//------------------------------------------------------------------------------

//! test ctor for non-spatial
template<typename T>
calc::VField<T>::VField(const   T& valueN,
                     size_t  size):
     d_spatial(false),
     d_valueNonSpatial(valueN),
     d_size(size),
     d_owningValueSpatial(0)
{
    d_value = &d_valueNonSpatial;
}

//! test ctor for spatial
template<typename T>
calc::VField<T>::VField(const   T* valueS,
                        size_t  size):
     d_value(valueS),
     d_spatial(true),
     d_size(size),
     d_owningValueSpatial(0)
{
}

//! ctor that deletes input arg \a f
/*!
 * \param f field that is transformed to this VField, deleted on return
 * \param size size simulated by the VField (not in f if f nonspatial)
 */
template<typename T>
calc::VField<T>::VField(Field   *f,size_t size):
   d_spatial(f->isSpatial()),
   d_size(size),
   d_owningValueSpatial(f)
{
  init(*f);
}

//! ctor that calls updateMVField
/*!
 * \param f field that is transformed to this VField
 * \param mvField after construction updateMVField is called with mvField
 *
 * mvField.size() is the size simulated by the created VField
 */
template<typename T>
calc::VField<T>::VField(const Field& f,BitField& mvField):
   d_spatial(f.isSpatial()),
   d_size(mvField.size()),
   d_owningValueSpatial(0)
{
  init(f);
  updateMVField(mvField);
}

//! ctor referencing to \a f
/*!
 * \param f field that is transformed to this VField
 * \param size size simulated by the VField
 */
template<typename T>
calc::VField<T>::VField(const Field& f,size_t size):
   d_spatial(f.isSpatial()),
   d_size(size),
   d_owningValueSpatial(0)
{
  init(f);
}

template<typename T>
void calc::VField<T>::init(const Field& f)
{
  PRECOND(crCode<T>()==f.cr());
  // TODO static_cast -> src_t
  if (!d_spatial) {
    d_valueNonSpatial=(static_cast<const T *>(f.src()))[0];
    d_value = &d_valueNonSpatial;
  } else {
    PRECOND(f.nrValues()==d_size);
    d_value=static_cast<const T *>(f.src());
  }
}

/*! \brief set \a mvField to 1 where this is MV, leave other \a mvField entries untouched
 *  \pre  mvField.size() == size()
 */
template<typename T>
void calc::VField<T>::updateMVField(BitField& mvField) const
{
  PRECOND(mvField.size() == size());
  if (d_spatial)
    for(size_t i=0; i<d_size; ++i)
      if (pcr::isMV(d_value[i]))
        mvField[i]=1;
}

/* NOT IMPLEMENTED
//! Copy constructor.
template<typename T>
calc::VField<T>::VField(VField const& rhs)

  : Base(rhs)

{
}
*/



template<typename T>
calc::VField<T>::~VField()
{
  delete d_owningValueSpatial;
}



/* NOT IMPLEMENTED
//! Assignment operator.
template<typename T>
calc::VField& calc::VField<T>::operator=(VField const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

template<typename T>
size_t calc::VField<T>::size() const {
     return d_size;
}

template<typename T>
bool calc::VField<T>::spatial() const {
     return d_spatial;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TEMPLATE INSTANCE
//------------------------------------------------------------------------------
namespace calc {
 template class VField<UINT1>;
 template class VField<INT4>;
 template class VField<REAL4>;
}
