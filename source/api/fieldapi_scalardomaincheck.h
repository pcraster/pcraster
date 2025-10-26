#ifndef INCLUDED_FIELDAPI_SCALARDOMAINCHECK
#define INCLUDED_FIELDAPI_SCALARDOMAINCHECK

#include "stddefx.h"
#include "com_interval.h"
#include "fieldapi_interface.h"

#include <string>
#include <vector>


namespace geo {
 class CellLoc;
}

namespace fieldapi {

//! test a ReadOnlyReal8 map to be in a certain domain
/*!
   used for scalar parameters of calc-lib functions if parameter have
   domain restrictions
*/
class ScalarDomainCheck
{

private:

   //  Copy operator.DEFAULT
   // ScalarDomainCheck(const ScalarDomainCheck&);

   //! ctor
   ScalarDomainCheck               ();

   //! map to test
   const ReadOnlyReal8&          d_map;
   //! parameter name for msg()
   const char                   *d_parName;

   //! the validating object
   const com::IntervalD* d_v;

public:
   //! Assignment constructor.
   ScalarDomainCheck& operator=(const ScalarDomainCheck& s);
   //! Copy constructor.
   ScalarDomainCheck(const ScalarDomainCheck& s);

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
   //! ctor
   ScalarDomainCheck(const ReadOnlyReal8& map,
      const char *parName, const com::IntervalD& v);


  /* virtual */    ~ScalarDomainCheck              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool valid(const geo::CellLoc& l) const;

   std::string msg() const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

int checkScalarDomains(
    const std::vector<ScalarDomainCheck>& v,
    const geo::CellLoc& loc);


} // namespace fieldapi

#endif
