#ifndef INCLUDED_PCRXML_SIMPLEATTR
#define INCLUDED_PCRXML_SIMPLEATTR

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRXML_ATTRIBUTE
#include "pcrxml_attribute.h"
#define INCLUDED_PCRXML_ATTRIBUTE
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

class QDomNode;
class QDomElement;

namespace pcrxml {

//! SimpleAttr as in simple type of XMLSchema WD
template<class T>class SimpleAttr : public Attribute
{

private:


  //! the value
  T d_value;

protected:
  std::string      attrValueStr() const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  SimpleAttr&      operator=               (const SimpleAttr&);
                   SimpleAttr              (const SimpleAttr&);
                   SimpleAttr              (const QDomNode& owningElement,
                                            const std::string& name, bool required);
                   SimpleAttr              (T   value);

                   SimpleAttr              ();

  /* virtual */   ~SimpleAttr              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  //! value of element
  T value() const { return d_value; };
  //! value of element
  T operator()() const { return d_value; };

};

typedef SimpleAttr<size_t> PositiveInteger;
typedef SimpleAttr<double> Double;

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
