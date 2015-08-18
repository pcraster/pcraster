#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_RUNSETTINGS
#include "calc_runsettings.h"
#define INCLUDED_CALC_RUNSETTINGS
#endif

// Library headers.
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_COMPLETEPARSER
#include "calc_completeparser.h"
#define INCLUDED_CALC_COMPLETEPARSER
#endif
#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h"
#define INCLUDED_CALC_POSITIONNAME
#endif


/*!
  \file
  This file contains the implementation of the RunSettings class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RUNSETTINGS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RUNSETTINGS MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::RunSettings::RunSettings()
{
}

//! construct by parsing a series of bindings from an ascii file \a bindingFile
calc::RunSettings::RunSettings(const com::PathName& bindingFile)
{
  CompleteParser<ASTNodeVector,com::PathName>  p(bindingFile);


  ASTNodeVector l;
  p.parse(&Parser::externalBindings,l);
  addLastDefinition(l);
}

//! dtor
calc::RunSettings::~RunSettings()
{
}


/* //! add new settings from \a mrsElement, keeping old values in case of duplicates
 * void calc::RunSettings::addNewOnly(
 *       QDomElement mrsElement)
 * {
 *
 * MOVE THIS INTO RUNDIRECTORY
 * make resulting map<ASTPar ,ASTId >
 *  maak van ASTPar en ASTID de position-name de directory naam 
 *   waar binding.ipcr is gevonden
 * BindingRewriter br;
 * pcrxml::forEachChildElement(mrsElement,br);
 * pcrxml::ModelRunSettings mrs(mrsElement);

 * // moved into binding by BindingRewriter
 * PRECOND(mrs.fileSetting.empty());
 * PRECOND(mrs.numericSetting.empty());

 * PositionName pn("RunSettings");

 * // insert does nothing if already there
 * // that is exactly what we want in this order of paths
 * for (size_t i=0; i < mrs.binding.size(); i++)
 *   d_bindings.insert(std::make_pair(
 *        Id(mrs.binding[i]->parameter(),&pn),
 *        Id(mrs.binding[i]->value(),&pn)));
 * }
 */

// //! create an xml elements from its contents
// pcrxml::ModelRunSettings *calc::RunSettings::createModelRunSettings() const
// {
//   pcrxml::ModelRunSettings *m=0;
//   try {
//     m=new pcrxml::ModelRunSettings();
//     PRECOND(FALSE);
// /*
//     for (Bindings::const_iterator pos=d_bindings.begin();
//          pos !=d_bindings.end(); ++pos) {
//             pcrxml::Binding* f(new pcrxml::Binding());
//             f->parameter=pos->first.name();
//             f->value=pos->second.name();
//             m->binding.push_back(f);
//     }
// */
//   } catch(...) {
//     delete m;
//     throw;
//   }
//   return m;
// }

/*
const calc::RunSettings::Bindings& calc::RunSettings::bindings() const
{
  return d_bindings;
}

//! add, overwrite possible existing one
void calc::RunSettings::add(const Id& name, const Id& value)
{
  d_bindings.erase(name);
  d_bindings.insert(std::make_pair(name,value));
}

//! erase all setting present and identical to \a parent
void calc::RunSettings::erase(const RunSettings& parent)
{
  com::erase(d_bindings,parent.d_bindings);
}

*/

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
