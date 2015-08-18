#ifndef INCLUDED_COM_MVOP
#define INCLUDED_COM_MVOP

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_STATIC_ASSERT
#include <boost/static_assert.hpp>
#define INCLUDED_BOOST_STATIC_ASSERT
#endif
#ifndef INCLUDED_BOOST_OPERATORS
#include <boost/operators.hpp>
#define INCLUDED_BOOST_OPERATORS
#endif


// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif


namespace com {
  // MVOp declarations.
}



namespace com {

// gcc 386 / x86_64 / msvc ok
#ifndef __i386__
#ifndef _MSC_VER
#ifndef __x86_64__
#error  CHECK ALL ASSUMPTIONS (Contact Cees)
#endif
#endif
#endif


//! operators that propagate a float and double MV correctly
/*!
 * DO NOT USE. see bugzilla #78
 * +,-,*
 *  IA32SPEC: Table E-1
 *  if some op is a QNan -> the same Qnan out
 * \todo
 *  check == in intel docs on MV propagation (Table E-2)
 * \todo
 *   more docs and for each op the opAss, as I have now addAss
 * \todo
 *   check some .S output for code size and efficiency
 */
template<typename T>
class MVOp
 : boost::addable< MVOp<T>         // MVOp + MvOp
 , boost::addable< MVOp<T>, T
 , boost::subtractable< MVOp<T>    // MVOp - MvOp
 , boost::subtractable< MVOp<T>, T
 , boost::multipliable< MVOp<T>    // MVOp * MvOp
 , boost::multipliable< MVOp<T>, T
 > > > > > >
{

  BOOST_STATIC_ASSERT(!std::numeric_limits<T>::is_integer);
  // long double not yet tested
  BOOST_STATIC_ASSERT(sizeof(T)==4||sizeof(T)==8);

  // no object size bloat allowed, only size of single value d_v
  // moved to void com::MVOpTest::testStaticAsserts()
  // here size not yet known, thus 0
  // BOOST_STATIC_ASSERT(sizeof(T)>=sizeof(MVOp<T>));

  friend class MVOpTest;

  T     d_v;

private:


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  MVOp&            operator=          (T v);
  MVOp&            operator=          (MVOp const& rhs);
  template<typename V>
                   MVOp               (V const&    v );
                   MVOp               (MVOp const& rhs);

                   MVOp               ();

                  ~MVOp               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void            setMV               ();
  MVOp            operator+=          (MVOp const& rhs);
  MVOp            operator+=          (T    const& rhs);
  MVOp            operator-=          (MVOp const& rhs);
//MVOp            operator-=          (T    const&  rhs);
  MVOp            operator*=          (MVOp const& rhs);
  MVOp            operator*=          (T    const& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const T&        operator()          () const;

  bool            isMV                () const;

  bool            operator==          (MVOp const& rhs) const;
  bool            operator==          (T rhs) const;

  char const*     debugBits           () const;

  //----------------------------------------------------------------------------
  // STATICS
  //----------------------------------------------------------------------------

  static T&       addAss(T& dest, const MVOp& src);
  static T&       subAss(T& dest, const MVOp& src);
};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------


template<typename T>
inline MVOp<T>& MVOp<T>::operator=(MVOp<T> const& rhs)
{
  // no if (this==&rhs)!
  d_v=rhs.d_v;
  return *this;
}

template<typename T>
inline MVOp<T>& MVOp<T>::operator=(T v)
{
  d_v=v;
  return *this;
}

template<typename T>
inline MVOp<T>::MVOp(MVOp<T> const& rhs):
  d_v(rhs.d_v)
{
}

//! ctor accepting only a value type V equal to T
/*!
 * Note: can not suffice with 1 value type T instead of two (V,T)
 * because we must forbid promotion of any type to T, which is done here with
 * a BOOST_STATIC_ASSERT
 *
 * \todo or can we do that by the explicit keyword?
 *
 * Note that compile constants must be suffix by f for
 * MVOp<float> as in MVOp<float> vop(1.0f)
 *
 * the value type restriction is forced at compilation
 * time by BOOST_STATIC_ASSERT's
 *
 */
template<typename T>
 template<typename V>
inline MVOp<T>::MVOp(V const& v):
   d_v(v)
{
  // see void com::MVOpTest::testImplicitCastStaticAsserts()
  // for testing these assertions
  BOOST_STATIC_ASSERT(sizeof(T) == sizeof(V));
  BOOST_STATIC_ASSERT(!std::numeric_limits<V>::is_integer);
}

template<typename T>
inline MVOp<T>::MVOp()
{
}

template<typename T>
inline MVOp<T>::~MVOp()
{
}


template<typename T>
inline MVOp<T> MVOp<T>::operator+=(MVOp const& rhs)
{
  d_v+=rhs.d_v;
  return *this;
}

template<typename T>
inline MVOp<T> MVOp<T>::operator+=(T const& rhs)
{
  d_v+=rhs;
  return *this;
}

template<typename T>
inline MVOp<T> MVOp<T>::operator-=(MVOp const& rhs)
{
  d_v-=rhs.d_v;
  return *this;
}

/*
template<typename T>
inline MVOp<T> MVOp<T>::operator-=(T const& rhs)
{
  d_v-=rhs;
  return *this;
}
*/

template<typename T>
inline MVOp<T> MVOp<T>::operator*=(MVOp const& rhs)
{
  d_v*=rhs.d_v;
  return *this;
}

template<typename T>
inline MVOp<T> MVOp<T>::operator*=(T const& rhs)
{
  d_v*=rhs;
  return *this;
}



template<typename T>
inline bool MVOp<T>::operator==(MVOp const& rhs) const
{
  return d_v==rhs.d_v;
}

template<typename T>
inline bool MVOp<T>::operator==(T rhs) const
{
  return d_v==rhs;
}

template<typename T>
inline const T& MVOp<T>::operator()() const
{
  return d_v;
}

/*
template<typename T>
inline bool MVOp<T>::isMV() const
{
  return pcr::isMV(d_v);
}
*/

template<>
inline bool MVOp<float>::isMV() const
{
  return IS_MV_REAL4(&d_v);
}

template<>
inline bool MVOp<double>::isMV() const
{
  return IS_MV_REAL8(&d_v);
}


template<typename T>
inline void com::MVOp<T>::setMV()
{
  pcr::setMV(d_v);
}

template<typename T>
inline char const* com::MVOp<T>::debugBits() const
{
  return (char const *)&d_v;
}
//------------------------------------------------------------------------------
// STATICS
//------------------------------------------------------------------------------

//! use non-const dest T with the add(+) operation
template<typename T>
inline T& MVOp<T>::addAss(T& dest, const MVOp& src) {
  dest += src.d_v;
  return dest;
}

//! use non-const dest T with the sub(-) operation
template<typename T>
inline T& MVOp<T>::subAss(T& dest, const MVOp& src) {
  dest -= src.d_v;
  return dest;
}

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace com

#endif
