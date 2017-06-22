#ifndef INCLUDED_DAL_TYPE
#define INCLUDED_DAL_TYPE



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif



namespace dal {
  // Type declarations.
}



namespace dal {



//! Interface to compile (template) type info
template<typename T>
 struct TypeTraits;

#if _MSC_VER == 1900
template<>
struct TypeTraits<UINT1> {
   static TypeId const typeId = TI_UINT1;
   static CSF_CR const csfCr = CR_UINT1;
 };

template<>
 struct TypeTraits<UINT2> {
   static TypeId const typeId = TI_UINT2;
   static CSF_CR const csfCr = CR_UINT2;
 };
template<>
 struct TypeTraits<UINT4> {
   static TypeId const typeId = TI_UINT4;
   static CSF_CR const csfCr = CR_UINT4;
 };

template<>
 struct TypeTraits<INT1> {
   static TypeId const typeId = TI_INT1;
   static CSF_CR const csfCr = CR_INT1;
 };

template<>
 struct TypeTraits<INT2> {
   static TypeId const typeId = TI_INT2;
   static CSF_CR const csfCr = CR_INT2;
 };
template<>
 struct TypeTraits<INT4> {
   static TypeId const typeId = TI_INT4;
   static CSF_CR const csfCr = CR_INT4;
 };
template<>
 struct TypeTraits<REAL4> {
   static TypeId const typeId = TI_REAL4;
   static CSF_CR const csfCr = CR_REAL4;
 };
template<>
 struct TypeTraits<REAL8> {
   static TypeId const typeId = TI_REAL8;
   static CSF_CR const csfCr = CR_REAL8;
 };
template<>
 struct TypeTraits<std::string> {
   static TypeId const typeId = TI_STRING;
 };
template<>
 struct TypeTraits<std::vector<UINT1> > {
   static TypeId const typeId = TI_UINT1_VECTOR;
 };
template<>
 struct TypeTraits<std::vector<INT4> > {
   static TypeId const typeId = TI_INT4_VECTOR;
 };
template<>
 struct TypeTraits<std::vector<REAL4> > {
   static TypeId const typeId = TI_REAL4_VECTOR;
 };
#else
template<>
struct PCR_DAL_DECL TypeTraits<UINT1> {
   static TypeId const typeId;
   static CSF_CR const csfCr;
 };

template<>
 struct PCR_DAL_DECL TypeTraits<UINT2> {
   static TypeId const typeId;
   static CSF_CR const csfCr;
 };
template<>
 struct PCR_DAL_DECL TypeTraits<UINT4> {
   static TypeId const typeId;
   static CSF_CR const csfCr;
 };

template<>
 struct PCR_DAL_DECL TypeTraits<INT1> {
   static TypeId const typeId;
   static CSF_CR const csfCr;
 };

template<>
 struct PCR_DAL_DECL TypeTraits<INT2> {
   static TypeId const typeId;
   static CSF_CR const csfCr;
 };
template<>
 struct PCR_DAL_DECL TypeTraits<INT4> {
   static TypeId const typeId;
   static CSF_CR const csfCr;
 };
template<>
 struct PCR_DAL_DECL TypeTraits<REAL4> {
   static TypeId const typeId;
   static CSF_CR const csfCr;
 };
template<>
 struct PCR_DAL_DECL TypeTraits<REAL8> {
   static TypeId const typeId;
   static CSF_CR const csfCr;
 };
template<>
 struct PCR_DAL_DECL TypeTraits<std::string> {
   static TypeId const typeId;
 };
template<>
 struct PCR_DAL_DECL TypeTraits<std::vector<UINT1> > {
   static TypeId const typeId;
 };
template<>
 struct PCR_DAL_DECL TypeTraits<std::vector<INT4> > {
   static TypeId const typeId;
 };
template<>
 struct PCR_DAL_DECL TypeTraits<std::vector<REAL4> > {
   static TypeId const typeId;
 };
#endif

//! Interface to compile (template) type info
template<TypeId T>
 struct TypeOfTypeId {};

template<>
struct TypeOfTypeId<TI_UINT1> {
 typedef UINT1 Type;
};

//! Interface to runtime type info
/*!
 * runtime use by static get:
 * \code
 *    TypeId v=foo();
 *    Type const& typeInfo(Type::get(v));
 * \endcode
 */
class Type
{

  friend class TypeTest;

private:
/*
  //! Assignment operator. NOT IMPLEMENTED.
  Type&            operator=           (Type const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Type                (Type const& rhs);
*/
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~Type               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  virtual TypeId   id                  () const=0;

  virtual bool     canParse            (std::string const& string) const=0;
  //! size of type
  /*!  e.g  Type::get(TI_REAL4).size() == sizeof(REAL4)
   */
  virtual size_t   size                ()const=0;

  virtual bool     hasTrivialCopy      ()const=0;

  static Type const& get               (TypeId ti);
/*
       isInteger()=0
       ..etc..
        size()=0 // sizeof(type)
*/
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



} // namespace dal

#endif
