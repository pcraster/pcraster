#ifndef INCLUDED_CALC_OBJECTLINKPROXY
#define INCLUDED_CALC_OBJECTLINKPROXY



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
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

// type1 links use std pcr types
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.
#ifndef INCLUDED_CALC_OBJECTLINK
#include "calc_objectlink.h"
#define INCLUDED_CALC_OBJECTLINK
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

namespace calc {
  // ObjectLinkProxy declarations.
  class Field;
}



namespace calc {


//! the simplest ObjectLink implementation (type 1) by using calcLibWrap
template<class C>
 class ObjectLinkProxy : public ObjectLink
{

  //! object doing the work
  C              *d_obj;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ObjectLinkProxy&           operator=           (ObjectLinkProxy const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ObjectLinkProxy               (ObjectLinkProxy const& rhs);

  //! specialization should implement this one
  bool             dispatch                      (const std::string&  methodName,
                                                  const std::vector<Field *>& fields);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ObjectLinkProxy               (const std::string&  stringArg,
                                                  const geo::RasterSpace&    rs,
                                                  size_t              nrFieldArgs);

     virtual       ~ObjectLinkProxy              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             exec1                    (const std::string&  methodName,
                                             const std::vector<Field *>& fields);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  static ObjectLink* create                 (const std::string&  stringArg,
                                             const geo::RasterSpace&    rs,
                                             size_t              nrFieldArgs);

  C const*         object                   () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------
template<class C>
inline ObjectLinkProxy<C>::ObjectLinkProxy(const std::string&   /* stringArg */,
                                           const geo::RasterSpace&    rs,
                                           size_t               /* nrFieldArgs */):
  d_obj(new C(rs))
{
}

template<class C>
inline ObjectLinkProxy<C>::~ObjectLinkProxy()
{
  delete d_obj;
}

template<class C>
inline void ObjectLinkProxy<C>::exec1(const std::string&  methodName,
                                      const std::vector<Field *>& fields)
{
  PRECOND(d_obj);
  if (!dispatch(methodName,fields))
    throw UnknownMethod();
}

template<class C>
ObjectLink* ObjectLinkProxy<C>::create(const std::string& stringArg,
                                       const geo::RasterSpace&   rs,
                                       size_t              nrFieldArgs)
{
  return new ObjectLinkProxy<C>(stringArg,rs,nrFieldArgs);
}

//! Returns the object doing all the work.
/*!
  \return    Object doing all the work.

  Used by the blockpy Python extension in order to call functions on the
  worker object which can't be wrapped by an object link (functions returning
  non-spatial results or user defined types). Extending the calcLibWrap script
  and connected functionality might make this function obsolete.
*/
template<class C>
C const* ObjectLinkProxy<C>::object() const
{
  return d_obj;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

#ifdef DEBUG_DEVELOP
# define TIE_FIELD_CHECK(f, spatial, type)     \
  DEVELOP_PRECOND(f->cri() == type);           \
  DEVELOP_PRECOND(f->isSpatial() == spatial);
#else
# define TIE_FIELD_CHECK(f, spatial, type)
#endif

// non spatial src/dest (dest  of vector does not work)
void tieField(UINT1& a, Field* f)
{
  TIE_FIELD_CHECK(f,false,CRI_1);
  a=f->src_1()[0];
}
void tieField(INT4& a, Field* f)
{
  TIE_FIELD_CHECK(f,false,CRI_4);
  a=f->src_4()[0];
}
void tieField(REAL4& a, Field* f)
{
  TIE_FIELD_CHECK(f,false,CRI_f);
  a=f->src_f()[0];
}


// spatial / input,src

void tieField(const UINT1*& a, Field* f)
{
  TIE_FIELD_CHECK(f,true,CRI_1);
  a=f->src_1();
}
void tieField(const INT4*& a, Field* f)
{
  TIE_FIELD_CHECK(f,true,CRI_4);
  a=f->src_4();
}
void tieField(const REAL4*& a, Field* f)
{
  TIE_FIELD_CHECK(f,true,CRI_f);
  a=f->src_f();
}

// spatial / result,dest

void tieField(UINT1*& a, Field* f)
{
  TIE_FIELD_CHECK(f,true,CRI_1);
  a=f->dest_1();
}
void tieField(INT4*& a, Field* f)
{
  TIE_FIELD_CHECK(f,true,CRI_4);
  a=f->dest_4();
}
void tieField(REAL4*& a, Field* f)
{
  TIE_FIELD_CHECK(f,true,CRI_f);
  a=f->dest_f();
}

#undef TIE_FIELD_CHECK

// single
template<typename T>
 void tieProxyArgument(T& a, Field* f)
 {
   tieField(a,f);
 }
// vector
template<typename T>
 void tieProxyArgument(std::vector<T>& a, Field* f)
 {
   a.push_back(T());
   tieField(a.back(),f);
 }


} // namespace calc

#endif
