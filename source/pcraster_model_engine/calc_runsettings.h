#ifndef INCLUDED_CALC_RUNSETTINGS
#define INCLUDED_CALC_RUNSETTINGS

#include "stddefx.h"
#include "calc_bindingtable.h"

#include <string>
#include <vector>
#include <map>


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

  /* virtual */    ~RunSettings              () override;

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
