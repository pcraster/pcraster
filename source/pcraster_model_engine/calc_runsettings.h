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

// Module headers.
#ifndef INCLUDED_CALC_BINDINGTABLE
#include "calc_bindingtable.h"
#define INCLUDED_CALC_BINDINGTABLE
#endif

namespace pcrxml {
//  class ModelRunSettings;
}
namespace com {
  class PathName;
}

namespace calc {



//! collection of setting used when running a script with the run directory setup
/*!
 * Like a ASTNodeVector of BindingTable, except that the last definition
 * is active, and multiple definitions are valid.
 */
class RunSettings : public BindingTable
{
public:
  // typedef std::map<Id,Id>      Bindings;

private:

  //! list of effective  external settings for model run
  // Bindings   d_bindings;


  // Assignment operator. DEFAULT
  // RunSettings&           operator=           (const RunSettings&);

  // Copy constructor. DEFAULT
  //               RunSettings               (const RunSettings&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RunSettings               ();

                   RunSettings               (const com::PathName& bindingFile);

  /* virtual */    ~RunSettings              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

/*
 * void addNewOnly(QDomElement mrsElement);
 * void clear();


 * void add(const Id& name, const Id& value);

 * void erase(const RunSettings& parent);
 */

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

/*
 * const Bindings&    bindings() const;
 *
 * pcrxml::ModelRunSettings *createModelRunSettings() const;
 */

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
