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
#ifndef INCLUDED_PCRXML_DOM
#include "pcrxml_dom.h"
#define INCLUDED_PCRXML_DOM
#endif
#ifndef INCLUDED_PCRXML_DOMALGORITHM
#include "pcrxml_domalgorithm.h"
#define INCLUDED_PCRXML_DOMALGORITHM
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRGENXML_MODELRUNSETTINGS
#include "pcrgenxml_modelrunsettings.h"
#define INCLUDED_PCRGENXML_MODELRUNSETTINGS
#endif

// Module headers.


/*!
  \file
  This file contains the implementation of the RunSettings class.
*/

namespace calc {
  //! rewrites NumericSetting and FileSetting elements into Binding element
  struct BindingRewriter {
    BindingRewriter() {}
    void operator()(QDomElement e) {
      if (e.tagName() == "NumericSetting" ||
          e.tagName() == "FileSetting") {
        e.setTagName("Binding");
        pcrxml::changeAttrName(e,"name","parameter");
        // FileSetting only:
        pcrxml::changeAttrName(e,"externalFileName","value");
      }
    }
  };
}

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

//! dtor
calc::RunSettings::~RunSettings()
{
}

//! add new settings from \a mrsElement, keeping old values in case of duplicates
void calc::RunSettings::addNewOnly(
      const QDomElement& mrsElement)
{
  BindingRewriter br;
  pcrxml::forEachChildElement(mrsElement,br);
  pcrxml::ModelRunSettings mrs(mrsElement);

  // moved into binding by BindingRewriter
  PRECOND(mrs.fileSetting.empty());
  PRECOND(mrs.numericSetting.empty());

  // insert does nothing if already there
  // that is exactly what we want in this order of paths
  for (auto & i : mrs.binding)
    d_bindings.insert(std::make_pair(
         i->parameter(),
         i->value()));
}

//! create an xml elements from its contents
pcrxml::ModelRunSettings *calc::RunSettings::createModelRunSettings() const
{
  pcrxml::ModelRunSettings *m=nullptr;
  try {
    m=new pcrxml::ModelRunSettings();
    for (const auto & d_binding : d_bindings) {
            auto* f(new pcrxml::Binding());
            f->parameter=d_binding.first.name();
            f->value=d_binding.second.name();
            m->binding.push_back(f);
    }
  } catch(...) {
    delete m;
    throw;
  }
  return m;
}

//! removes all settings
void calc::RunSettings::clear()
{
  d_bindings.clear();
}

const calc::RunSettings::Bindings& calc::RunSettings::bindings() const
{
  return d_bindings;
}

//! add, overwrite possible existing one
void calc::RunSettings::add(const ExtSym& name, const ExtSym& value)
{
  d_bindings.erase(name);
  d_bindings.insert(std::make_pair(name,value));
}

//! erase all setting present and identical to \a parent
void calc::RunSettings::erase(const RunSettings& parent)
{
  com::erase(d_bindings,parent.d_bindings);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
