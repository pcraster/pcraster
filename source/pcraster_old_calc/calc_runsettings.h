#ifndef INCLUDED_CALC_RUNSETTINGS
#define INCLUDED_CALC_RUNSETTINGS

#include "stddefx.h"
#include "pcrdll.h"
#include "calc_extsym.h"

#include <string>
#include <vector>
#include <map>



namespace pcrxml {
  class ModelRunSettings;
}

class QDomElement;


namespace calc {



//! collection of setting used when running a script with the run directory setup
/*!
 * Holds a single unique collection of (name,value) tuples. aka bindings
 */
class RunSettings
{
public:
  typedef std::map<ExtSym,ExtSym>      Bindings;

private:

  //! list of effective  external settings for model run
  Bindings   d_bindings;


  // Assignment operator. DEFAULT
  // RunSettings&           operator=           (const RunSettings&);

  // Copy constructor. DEFAULT
  //               RunSettings               (const RunSettings&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RunSettings               ();

  /* virtual */    ~RunSettings              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void clear();

  void addNewOnly(const QDomElement& mrsElement);

  void add(const ExtSym& name, const ExtSym& value);

  void erase(const RunSettings& parent);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const Bindings&    bindings() const;

  pcrxml::ModelRunSettings *createModelRunSettings() const;

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
