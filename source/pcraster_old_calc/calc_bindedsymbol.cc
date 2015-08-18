#include "stddefx.h"

#ifndef INCLUDED_CALC_BINDEDSYMBOL
#include "calc_bindedsymbol.h"
#define INCLUDED_CALC_BINDEDSYMBOL
#endif

#ifndef INCLUDED_PCRGENXML_DATA
#include "pcrgenxml_data.h"
#define INCLUDED_PCRGENXML_DATA
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

//! looks in binding table if binding is avail
calc::BindedSymbol::BindedSymbol(const calc::Symbol& parName):
  Symbol(parName)
{
  const Symbol *b = scriptConst().findBinding(name());
  if (b)
     setBinded(*b);
  else {
     d_externalBinding = parName;
     d_userName        = name();
     d_externalName    = name();
  }
}

//! construct with binding (happens in ArrayDefinition)
/*!
 *  used in calc::ParsIndexName::addMe()
 */
calc::BindedSymbol::BindedSymbol(
  const Symbol& parName,
  const Symbol& bindedTo):
    Symbol(parName)
{
  setBinded(bindedTo);
}

void calc::BindedSymbol::setBinded(const Symbol& bindedTo)
{
  d_externalBinding = bindedTo;
  d_externalName    = bindedTo.name();
  setBindingInUserName();
}

//! include binding in user presented name
void calc::BindedSymbol::setBindingInUserName()
{
  d_userName = name() + "(binding=" + externalName() + ")";
}

//! set path to input file
/*!
    modifies externalName() if found in a search path
    as implemented in calc::RunDirectory::inputFilePath()
    \todo
      refactor to input symbols, that has this one automatic
 */
void calc::BindedSymbol::setInputFilePath()
{
  // d_externalBinding = d_externalName;
  d_externalName = scriptConst().inputFilePath(d_externalName);
  if (d_externalName != name())
     setBindingInUserName();
}

//! the name used to look for externally
/*! default this is equal to the name(), except
 *  if the symbol has a binding
 *  if -r is used this will become an absolute path
 *  \sa
 *      d_externalBinding
 */
const std::string& calc::BindedSymbol::externalName() const
{
  return d_externalName;
}

//! the name presented to user in case of external failures
/*! default this is equal to the name(), except
 *  if the symbol has a binding, then it returns
 *    name() + "(binding=" + externalName() + ")"
 */
const std::string& calc::BindedSymbol::userName() const
{
  return d_userName;
}

//! throw posError() formatted as userName()+": "+excep.messages()
void  calc::BindedSymbol::symError(const com::Exception& excep) const
{
 posError(userName()+": "+excep.messages());
}

void  calc::BindedSymbol::setName(pcrxml::Data *d)const
{
   d->name = name();
   if (name() != d_externalBinding.name())
     d->externalFileName = d_externalBinding.name();
}

/*!
 * \todo
 *   make it more a parameter to posError that decides if binded
 *   is preferred, for example input file not found always on binding
 *   see also test372a; describe message from the operand not the operators!
 */
void  calc::BindedSymbol::posError(const std::string& msg) const
{
  if (d_externalBinding.positionPriority() > positionPriority())
    d_externalBinding.posError(msg);
  else
    Element::posError(msg);
}

void  calc::BindedSymbol::posError(const std::ostringstream& msg) const
{
  posError(msg.str());
}
