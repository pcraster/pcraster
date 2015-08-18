#ifndef INCLUDED_FIELDAPI_SCALARDOMAINCHECK
#define INCLUDED_FIELDAPI_SCALARDOMAINCHECK



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

// PCRaster library headers.
#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif

// Module headers.
#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif


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
