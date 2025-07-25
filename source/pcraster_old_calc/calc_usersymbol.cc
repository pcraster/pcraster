#include "stddefx.h"
#include "calc_usersymbol.h"
#include "calc_iscript.h"
#include "calc_infoscript.h"
#include "calc_vs.h" // toString()

//! ctor
calc::UserSymbol::UserSymbol(
 const BindedSymbol& parName):
 BindedSymbol(parName)
{
}

//! dtor
calc::UserSymbol::~UserSymbol()
{
}

void calc::UserSymbol::goInScope()
{
}

void calc::UserSymbol::finalCheck()
{
}

//! see symbolSequenceNr()
/*!
 * SymbolTable calls this one after insertion
 */
void calc::UserSymbol::setSymbolSequenceNr(
    int symbolSequenceNr)
{
    d_symbolSequenceNr=symbolSequenceNr;
}

//! number to remember the definition order of symbols 
int calc::UserSymbol::symbolSequenceNr() const
{
    return d_symbolSequenceNr;
}

calc::UserSymbol *calc::UserSymbol::copyContents(
 const calc::ParsPar& ) const
{
 return nullptr;
}

void calc::UserSymbol::print(calc::InfoScript& i)const
{
 i.stream() << "<A NAME=\"" << name() << "\">" << name() << "<BR>";
 if (userName() != externalName()) {
  i.stream() << "userName:" << userName();
  i.stream() << " extName:" << externalName();
 }
 i.stream() << "vs:" << toString(symbolType()) << "<BR>";
 printSpecific(i);
}

void calc::UserSymbol::printSpecific(calc::InfoScript& )const
{
 POSTCOND(FALSE);
}

//! create xml Data element for some parameters
/*!
  Return 0 if no info creation implemented.
  Currently it does NOT create info for all elements,
  all array stuff is skipped
 */
pcrxml::Data *calc::UserSymbol::createXmlData() const
{
  return nullptr;
}
