#ifndef INCLUDED_DAL_TYPE
#include "dal_Type.h"
#define INCLUDED_DAL_TYPE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_BASICTYPES
#include "dal_BasicTypes.h"
#define INCLUDED_DAL_BASICTYPES
#endif

#ifndef INCLUDED_DAL_TYPES
#include "dal_Types.h"
#define INCLUDED_DAL_TYPES
#endif


/*!
  \file
  This file contains the implementation of the Type and Types class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TYPE MEMBERS
//------------------------------------------------------------------------------

namespace dal {

 /* these statistics are simple constructible
  * and only return from get(), no need to worry
  * about initialization
  */
 static Uint1Type  uint1Type;
 static Uint2Type  uint2Type;
 static Uint4Type  uint4Type;
 static Int1Type   int1Type;
 static Int2Type   int2Type;
 static Int4Type   int4Type;
 static Real4Type  real4Type;
 static Real8Type  real8Type;
 static StringType stringType;

#if _MSC_VER != 1900
  TypeId const TypeTraits<UINT1>::typeId = TI_UINT1;
  CSF_CR const TypeTraits<UINT1>::csfCr  = CR_UINT1;
  TypeId const TypeTraits<UINT2>::typeId = TI_UINT2;
  CSF_CR const TypeTraits<UINT2>::csfCr  = CR_UINT2;
  TypeId const TypeTraits<UINT4>::typeId = TI_UINT4;
  CSF_CR const TypeTraits<UINT4>::csfCr  = CR_UINT4;
  TypeId const TypeTraits<INT1>::typeId = TI_INT1;
  CSF_CR const TypeTraits<INT1>::csfCr  = CR_INT1;
  TypeId const TypeTraits<INT2>::typeId = TI_INT2;
  CSF_CR const TypeTraits<INT2>::csfCr  = CR_INT2;
  TypeId const TypeTraits<INT4>::typeId = TI_INT4;
  CSF_CR const TypeTraits<INT4>::csfCr  = CR_INT4;
  TypeId const TypeTraits<REAL4>::typeId = TI_REAL4;
  CSF_CR const TypeTraits<REAL4>::csfCr  = CR_REAL4;
  TypeId const TypeTraits<REAL8>::typeId = TI_REAL8;
  CSF_CR const TypeTraits<REAL8>::csfCr  = CR_REAL8;
  TypeId const TypeTraits<std::string>::typeId = TI_STRING;
  TypeId const TypeTraits<std::vector<UINT1> >::typeId = TI_UINT1_VECTOR;
  TypeId const TypeTraits<std::vector<INT4> >::typeId = TI_INT4_VECTOR;
  TypeId const TypeTraits<std::vector<REAL4> >::typeId = TI_REAL4_VECTOR;
#endif
}

/*
template<>
 struct TypeTraits<std::vector<UINT1> > {
   static TypeId const typeId =TI_UINT1_VECTOR;
 };
template<>
 struct TypeTraits<std::vector<INT4> > {
   static TypeId const typeId =TI_INT4_VECTOR;
 };
template<>
 struct TypeTraits<std::vector<REAL4> > {
   static TypeId const typeId =TI_REAL4_VECTOR;
 };
 */


//------------------------------------------------------------------------------
// DEFINITION OF TYPE MEMBERS
//------------------------------------------------------------------------------

dal::Type::~Type()
{
}


/* NOT IMPLEMENTED
//! Assignment operator.
dal::Type& dal::Type::operator=(Type const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/


dal::Type const& dal::Type::get(
         TypeId ti)
{
  switch(ti) {
    case TI_INT1 : return int1Type;
    case TI_INT2 : return int2Type;
    case TI_INT4 : return int4Type;
    case TI_UINT1: return uint1Type;
    case TI_UINT2: return uint2Type;
    case TI_UINT4: return uint4Type;
    case TI_REAL4: return real4Type;
    case TI_REAL8: return real8Type;
    case TI_STRING:return stringType;
    default: assert(false); return stringType;
 }
}

dal::Types::Types()
{
  push_back(&uint1Type);
  push_back(&uint2Type);
  push_back(&uint4Type);
  push_back(&int1Type);
  push_back(&int2Type);
  push_back(&int4Type);
  push_back(&real4Type);
  push_back(&real8Type);
  // push_back(&stringType);
}

dal::Types::~Types()
{
}



dal::TypeId dal::Types::idOfSmallestType(
         std::string const& string) const
{
  for(const_iterator it = begin(); it != end(); ++it) {
    if((**it).canParse(string)) {
      return (**it).id();
    }
  }

  return TI_NR_TYPES;
}



dal::TypeId dal::Types::idOfLargestType(TypeId id1, TypeId id2) const
{
  TypeId id = TI_NR_TYPES;

  for(const_iterator it = begin(); it != end(); ++it) {
    if((**it).id() == id1) {
      id = id2;
      break;
    }
    else if((**it).id() == id2) {
      id = id1;
      break;
    }
  }

  assert(id != TI_NR_TYPES);

  return id;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

