/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_DATATYPEENUM
#define INCLUDED_PCRGENXML_DATATYPEENUM


#ifndef INCLUDED_PCRXML_ATTRIBUTE
#include "pcrxml_attribute.h"
#define INCLUDED_PCRXML_ATTRIBUTE
#endif




class QDomElement;

namespace pcrxml{
class DataTypeEnum : public Attribute {
  public:
   typedef enum EnumType {
     Tss=0,Table=1,Field=2,Unknown=3,Boolean=4,Nominal=5,Ordinal=6,Scalar=7,Directional=8,Ldd=9
   } EnumType;
  private:
   EnumType d_value;
  public:
   DataTypeEnum();
   DataTypeEnum(EnumType value);
   DataTypeEnum(
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
   /*! use is confusing, !(DataTypeEnum == EnumType) better?
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
