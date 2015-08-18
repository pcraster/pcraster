#ifndef INCLUDED_PCRXML_ENUMNMTOKEN
#define INCLUDED_PCRXML_ENUMNMTOKEN

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_PCRXML_STRING
#include "pcrxml_string.h" // used in value()
#define INCLUDED_PCRXML_STRING
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

class QDomNode;

namespace pcrxml {


template<class T> class EnumNmToken {
private:
  //! list with NMTOKEN values
  static const char * d_strings[];

  //! \a d_strings list size
  static const size_t d_nrStrings;

  //! assignment ctor ctor NOT IMPLEMENTED
  EnumNmToken&           operator=           (const EnumNmToken&);

  //! copy ctor NOT IMPLEMENTED
                   EnumNmToken               (const EnumNmToken&);

public:
  //! type of enum
  typedef typename T::EnumType EnumType;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
 // need def. ctor. for creating const objects?
 EnumNmToken() {};

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------


  //! find atttribute named \name in \a owningElement and return C++ enum \a EnumType
  /*!
      attribute must be present
   */
  EnumType value(const QDomNode& owningElement, const std::string& name) const
  {
    // create string Attribute and use that value
    String str(owningElement,name,true);
    // should not be called otherwise
    PRECOND(str.present());
    // size_t i=lookup(str,d_strings,d_nrValues); dunno non-inline
    size_t i;
    for (i=0; i < d_nrStrings && (str.value() != d_strings[i]); i++)
       ;
    if (i == d_nrStrings)
      throw com::BadStreamFormat("attribute "+name+": "+str.value()+" is not a defined NMTOKEN value");
    return static_cast<EnumType>(i);
  }

  //! string value
  std::string attrValueStr(EnumType value) const
  {
      size_t i=static_cast<size_t>(value);
      PRECOND(i < d_nrStrings);
      String str(d_strings[i]);
      return str.value();
  }
};


//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pcrxml

#endif
