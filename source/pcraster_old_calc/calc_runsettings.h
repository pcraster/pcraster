#ifndef INCLUDED_CALC_RUNSETTINGS
#define INCLUDED_CALC_RUNSETTINGS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

// Module headers.
#ifndef INCLUDED_CALC_EXTSYM
#include "calc_extsym.h"
#define INCLUDED_CALC_EXTSYM
#endif



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

  void addNewOnly(QDomElement mrsElement);

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
