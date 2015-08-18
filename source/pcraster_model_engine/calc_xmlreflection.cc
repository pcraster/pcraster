#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_XMLREFLECTION
#include "calc_xmlreflection.h"
#define INCLUDED_CALC_XMLREFLECTION
#endif

// Library headers.
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif
// PCRaster library headers.
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
#ifndef INCLUDED_CALC_IOSTRATEGY
#include "calc_iostrategy.h"
#define INCLUDED_CALC_IOSTRATEGY
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif
// Module headers.



/*!
  \file
  This file contains the implementation of the XMLReflection class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class XMLReflectionPrivate
{
public:

  XMLReflectionPrivate()
  {
  }

  ~XMLReflectionPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC XMLREFLECTION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF XMLREFLECTION MEMBERS
//------------------------------------------------------------------------------

calc::XMLReflection::XMLReflection(
   ASTScript const& script):
  d_script(new pcrxml::Script())
{
  // add all definition's of symbols that can be described
  ASTSymbolTable const& syms(script.symbols());
  for (ASTSymbolTable::const_iterator pos=syms.begin();
            pos!=syms.end(); ++pos) {
    std::auto_ptr<pcrxml::Definition> d(pos->second.createDefinition());
    if (d.get())
     d_script->definition().push_back(*d);
  }
}

/* NOT IMPLEMENTED
//! Copy constructor.
calc::XMLReflection::XMLReflection(XMLReflection const& rhs)

  : Base(rhs)

{
}
*/

calc::XMLReflection::~XMLReflection()
{
  delete d_script;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::XMLReflection& calc::XMLReflection::operator=(XMLReflection const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

std::string calc::XMLReflection::toString() const
{
  std::ostringstream s;
  pcrxml::script(s,*d_script,pcrxsd::namespaceInfoMap("PCRaster.xsd"));
  return s.str();
}

/*
 * bool calc::XMLReflection::ambiguous() const
 * {
 *   for(size_t i=0; i < d_em->exchangeItem.size();++i) {
 *      pcrxml::Variable *v= d_em->exchangeItem[i]->variable;
 *      if (pcrxml::Spatial::Either== v->spatial())
 *        return true;
 *      // TODO check on 1 DataType
 *      PRINT_VAR(1);
 *   }
 *   return false;
 * }
 */

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



