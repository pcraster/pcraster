#ifndef INCLUDED_PCRXML_STRING
#define INCLUDED_PCRXML_STRING



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_PCRXML_ATTRIBUTE
#include "pcrxml_attribute.h"
#define INCLUDED_PCRXML_ATTRIBUTE
#endif

class QDomNode;

namespace pcrxml {

//! string attribute
/*!
 * An attribute value as a string, empty strings are allowed
 */
class String : public Attribute
{

protected:
  //! the value, empty strings are allowed
  std::string          d_value;

private:
  //  Assignment operator. DEFAULT
  // String&           operator=           (const String&);

  // Copy constructor. Default will do
  //             String               (const String&);


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                     String               (const QDomNode& owningElement,
                                               const std::string& name, bool required);
                     String               (const std::string& value);
                     String               (const char*        value);
//String&            operator=            (const std::string& value);
                     String               ();

  virtual           ~String               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! the value, empty strings are allowed
  const std::string&  value() const { return d_value; }

  //! the value, empty strings are allowed
  const std::string&  operator()() const { return d_value; }

  std::string attrValueStr() const;
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
