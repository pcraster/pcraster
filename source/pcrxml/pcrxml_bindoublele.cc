#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_BINDOUBLELE
#include "pcrxml_bindoublele.h"
#define INCLUDED_PCRXML_BINDOUBLELE
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_IOMANIP
#include <iomanip>
#define INCLUDED_IOMANIP
#endif
// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the BinDoubleLE class.
*/



//------------------------------------------------------------------------------

/*
namespace pcrxml {

class BinDoubleLEPrivate
{
public:

  BinDoubleLEPrivate()
  {
  }

  ~BinDoubleLEPrivate()
  {
  }

};

} // namespace pcrxml
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC BINDOUBLELE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BINDOUBLELE MEMBERS
//------------------------------------------------------------------------------

pcrxml::BinDoubleLE::BinDoubleLE(const QDomNode& owningElement,
                                 const std::string& name, bool required):
  Attribute(owningElement,name,required)
{
  if (!present())
    return;
  d_value=hexToDouble(inputValueStr(owningElement, name));
}

pcrxml::BinDoubleLE::BinDoubleLE(double value):
     Attribute(true),
     d_value(value)
{
}

double pcrxml::BinDoubleLE::hexToDouble (const std::string& hexString)
{
  double v;

  PRECOND(hexString.size() == sizeof(double)*2);
  unsigned char *ptr=(unsigned char *)&v;
  for(size_t i=0;i < sizeof(double); i++) {
    std::istringstream  is(hexString.substr(i*2,2));
    int value;
    is >> std::hex >>  value;
    ptr[i]=value;
  }
  return v;
}

//! ctor
pcrxml::BinDoubleLE::BinDoubleLE():
     Attribute(false) 
{
}

//! dtor
pcrxml::BinDoubleLE::~BinDoubleLE()
{
}

std::string pcrxml::BinDoubleLE::attrValueStr() const
{
  const unsigned char *ptr=(const unsigned char *)&d_value;
  std::ostringstream  o;
  for(size_t i=0;i < sizeof(double); ++i) {
    o << std::hex << std::setw(2) << std::setfill('0') <<  static_cast<int>(ptr[i]);
  }
  POSTCOND(o.str().size() == sizeof(double)*2);
  return o.str();
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



