#ifndef INCLUDED_CALC_WLDELFTHABITAT
#define INCLUDED_CALC_WLDELFTHABITAT

#include "stddefx.h"
#include "com_pathname.h"
#include "com_uniquestringgenerator.h"
#include "calc_modelbuilder.h"

#include <map>
#include <utility>


class QDomElement;



namespace calc {

class  StatTable;

//! specific to Wl Delft Habitat project
/*!
 * build a model from a project specific xml script
 */
class WlDelftHabitat: public ModelBuilder
{
private:

  //! Assignment operator. NOT IMPLEMENTED.
  WlDelftHabitat&           operator=           (const WlDelftHabitat&);

  //! Copy constructor. NOT IMPLEMENTED.
                   WlDelftHabitat               (const WlDelftHabitat&);

  com::PathName   d_xmlFile;

  struct Parameter {
    const std::string d_name;
    //! if d_report true, then there is a binding for d_name
    const bool        d_report;
    Parameter(std::string name, bool report):
      d_name(std::move(name)),d_report(report) {}
  };
  //! keeps track of symbol found in xml file plus generated ones
  com::UniqueStringGenerator  d_alreadyDefinedSymbols;

  //! store identifier->parameter pairs, to find
  typedef std::map<std::string, Parameter> Identifier2Name;
  Identifier2Name                          d_identifier2Name;

  void parseRules(const QDomElement&  model, const std::string& tagName);

  void addKlassen      (const QDomElement& rule);
  void addLookuplinear (const QDomElement& rule);
  void addExpr         (const QDomElement& rule, const std::string& tagWithExpr);
  void addRuleTableExpr(const QDomElement& rule, const std::string& function,
                        LookupTable  *tab);

  const Parameter& parameter(const std::string& reference) const;
  const Parameter& parameter(const QDomElement& reference) const;

  const Parameter& input (const QDomElement& rule) const;
  const Parameter& output(const QDomElement& rule) const;

  void  addAssignment(const Parameter& assignTo, const std::string& expr);
  void  parseParameter(const QDomElement&  de);
  void  parseModel(const QDomElement&   de);
  void  parseEcotoop(const QDomElement&  de);
  void  parseStatistics(const QDomElement&  de);

  std::string generatedName(const std::string& nameSuggestion);

  std::string addInlineTable(
                  const std::string& nameSuggestion,
                  LookupTable  *tab);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   WlDelftHabitat               (const com::PathName& xmlFile);

  /* virtual */    ~WlDelftHabitat              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              parseXml();
  StatTable*        addStatistics               (const std::string& subjectName);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
