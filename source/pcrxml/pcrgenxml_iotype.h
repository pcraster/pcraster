/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_PCRGENXML_IOTYPE
#define INCLUDED_PCRGENXML_IOTYPE


#ifndef INCLUDED_PCRXML_ATTRIBUTE
#include "pcrxml_attribute.h"
#define INCLUDED_PCRXML_ATTRIBUTE
#endif




class QDomElement;

namespace pcrxml{
class IoType : public Attribute {
  public:
   typedef enum EnumType {
     Input=0,Output=1,Constant=2,Both=3,None=4
   } EnumType;
  private:
   EnumType d_value;
  public:
   IoType();
   IoType(EnumType value);
   IoType(
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
   /*! use is confusing, !(IoType == EnumType) better?
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
