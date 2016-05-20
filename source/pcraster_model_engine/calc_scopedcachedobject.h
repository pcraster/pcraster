#ifndef INCLUDED_CALC_SCOPEDCACHEDOBJECT
#define INCLUDED_CALC_SCOPEDCACHEDOBJECT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif



namespace calc {
  // ScopedCachedObject declarations.
}



namespace calc {


//! Scoped use of an object stored  RunTimeEnv::d_cache
/*!
 * solves the problem (by design?) the Field's on the stack may
 * be the only reference of a "parameter", parameter value is already deleted
 * in the DataTable when pushing this last use on stack.
 */
template<typename O>
class ScopedCachedObject
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ScopedCachedObject&           operator=     (ScopedCachedObject const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ScopedCachedObject          (ScopedCachedObject const& rhs);

  RunTimeEnv*      d_rte;
  const Field*     d_field;
  const O*         d_object;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ScopedCachedObject               (RunTimeEnv *rte,
                                                     const Field *field);

                   ~ScopedCachedObject              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setObject                        (const O* object);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const O*         object                           () const;

};


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename O>
ScopedCachedObject<O>::ScopedCachedObject(
  RunTimeEnv *rte, const Field *field):
  d_rte(rte),
  d_field(field)
{
  d_object   = dynamic_cast<const O *>(rte->cachedObject(field->src()));
}

template<typename O>
void ScopedCachedObject<O>::setObject(
  const O* object)
{
  d_object   = object;
}

template<typename O>
const O* ScopedCachedObject<O>::object() const
{
  return d_object;
}

//! go out of scrope
template<typename O>
ScopedCachedObject<O>::~ScopedCachedObject()
{
  if (d_field->readOnlyReference()) {
    // there are more references that can take advantage
    // of this cached object
    d_rte->transferIfCached(d_field->src(),d_object);
    if (d_rte->cachedObject(d_field->src()) != d_object) {
      // d_object is not managed and deleted by d_rte
      // since caching is disabled
      delete d_object;
    }
  } else {
    // d_field is only copy left, can delete d_object, will never be used again
    d_rte->deleteCacheEntry(d_field->src());
    delete d_object;
  }
  d_object=0;
}

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
