#ifndef INCLUDED_PCRXML_ATTRIBUTE
#define INCLUDED_PCRXML_ATTRIBUTE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

class QDomNode;
class QDomElement;

namespace pcrxml {

//! Attribute of an element
/*!
   Models an attribute of an element. Since attribute can be optional,
   e.g. not required, a present() property is used, to test if the 
   attribute is defined.
 */
class Attribute
{

private:
  //! Is attribute present
  bool             d_present;


  //  Assignment operator. DEFAULT
  //  Attribute&           operator=           (const Attribute&);

  // Copy constructor. DEFAULT
  //             Attribute               (const Attribute&);

protected:
   static std::string inputValueStr    (const QDomNode& owningElement,
                                        const std::string& nameOfAttr);

                   Attribute           (bool present);
  inline void      setPresent          (bool present);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Attribute               (const QDomNode& owningElement,
                                            const std::string& nameOfAttr, bool required);

     virtual       ~Attribute              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! add attribute to  element \a e under name \a name
  void               addToElement    (QDomElement& e,
                                      const std::string& name) const;

   //! return my value as a the XML Attr Value string
   virtual std::string       attrValueStr() const=0;

  bool               present          () const;
};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

//! set value of d_present
inline void Attribute::setPresent(bool present)
{
  d_present=present;
}

//! get value of d_present
inline bool Attribute::present() const
{
  return d_present;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pcrxml

#endif
