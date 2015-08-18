#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_SIMPLEATTR
#include "pcrxml_simpleattr.h"
#define INCLUDED_PCRXML_SIMPLEATTR
#endif

// Library headers.
#ifndef  INCLUDED_QDOM
#include <qdom.h>
#define  INCLUDED_QDOM
#endif
#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_PCRXML_STRINGCONV
#include "pcrxml_stringconv.h"
#define INCLUDED_PCRXML_STRINGCONV
#endif

/*!
  \file
  This file contains the implementation of the SimpleAttr class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SIMPLEATTR MEMBERS
//------------------------------------------------------------------------------
namespace pcrxml {

//! init type from String
template<class T>T fromString(
      const std::string& str);

//! init bool from String
template<>bool fromString<bool>(
      const std::string& str)
{ return com::strToBool(str); }
//! init size_t from String
template<>size_t fromString<size_t>(
      const std::string& str)
{ return com::strToSize_t(str); }
template<>int fromString<int>(
      const std::string& str)
{ return com::strToInt(str); }
//! init double from String
template<>double fromString<double>(
      const std::string& str)
{ return com::strToDouble(str); }

//! string version of type T
template<class T>std::string toString(T v);

//! string version of bool
template<>std::string toString<bool>(bool v)
{ return com::boolToStr(v); }
//! string version of size_t
template<>std::string toString<size_t>(size_t v)
{ return com::size_tToStr(v); }
template<>std::string toString<int>(int v)
{ return com::intToStr(v); }
//! string version of double
template<>std::string toString<double>(double v)
{ return com::doubleToStr(v); }

}

//------------------------------------------------------------------------------
// DEFINITION OF SIMPLEATTR MEMBERS
//------------------------------------------------------------------------------

//! ctor from DOM Tree
template<class T>
  pcrxml::SimpleAttr<T>
  ::SimpleAttr(
    const QDomNode&    owningElement,
    const std::string& name,
    bool               required):
  Attribute(owningElement,name,required)
{
  if (!present())
    return;
  try {
   d_value = fromString<T>(inputValueStr(owningElement, name));
  } catch (const std::range_error& ) {
    throw com::BadStreamFormat(
         asString(owningElement.nodeName())+":"+name+": invalid attribute value");
  }
}

//! ctor from value
template< class T>
  pcrxml::SimpleAttr<T>
  ::SimpleAttr(T value):
  Attribute(true),
  d_value(value)
{
}

//! ctor
template< class T>
  pcrxml::SimpleAttr<T>
  ::SimpleAttr():
  Attribute(false)
{
}
//! ctor
template< class T>
  pcrxml::SimpleAttr<T>
  ::SimpleAttr(const SimpleAttr& rhs):
  Attribute(rhs)
{
  if (rhs.present())
    d_value=rhs.d_value;
}

//! Assignment operator.
template< class T>
pcrxml::SimpleAttr<T>&
pcrxml::SimpleAttr<T>::operator=(const SimpleAttr& rhs)
{
 if (this != &rhs) {
   if (rhs.present()) {
    setPresent(true);
    d_value=rhs.d_value;
   }
 }
 return *this;
}


//! dtor
template< class T>
  pcrxml::SimpleAttr<T>
  ::~SimpleAttr()
{
}

template< class T>
 std::string
  pcrxml::SimpleAttr<T>
  ::attrValueStr() const
{
  return toString<T>(value());
}

template class pcrxml::SimpleAttr<bool>;
template class pcrxml::SimpleAttr<size_t>;
template class pcrxml::SimpleAttr<int>;
template class pcrxml::SimpleAttr<double>;

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
