/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_SCRIPTTYPE
#define INCLUDED_PCRGENXML_SCRIPTTYPE


#ifndef INCLUDED_PCRXML_ATTRIBUTE
#include "pcrxml_attribute.h"
#define INCLUDED_PCRXML_ATTRIBUTE
#endif




class QDomElement;

namespace pcrxml{
class ScriptType : public Attribute {
  public:
   typedef enum EnumType {
     Static=0,Dynamic=1
   } EnumType;
  private:
   EnumType d_value;
  public:
   ScriptType();
   ScriptType(EnumType value);
   ScriptType(
    const QDomNode& owningElement, const std::string& nameOfAttr, bool req);

   //! return value
   EnumType  operator()() const
        { return value(); };


   //! return value
   EnumType  value()      const
        { PRECOND(present());return d_value; };

   //! return enum value as string, (?? empty if not present ??)
   std::string attrValueStr() const;

   //! is value present and set to \a value ?
   bool operator==(EnumType value) const {
        return present()&& d_value==value;
   };

   //! is value not present or not equal to \a value ?
   /*! use is confusing, !(ScriptType == EnumType) better?
    */
   bool operator!=(EnumType value) const {
        return !present() || d_value!=value;
   };

   void set(EnumType value) {
     d_value=value;
     setPresent(true);
   }
   void operator=(EnumType value) {
     set(value);
   }

  };
}


#endif
