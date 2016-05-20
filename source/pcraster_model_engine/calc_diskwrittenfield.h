#ifndef INCLUDED_CALC_DISKWRITTENFIELD
#define INCLUDED_CALC_DISKWRITTENFIELD


#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_DATAVALUE
#include "calc_datavalue.h"
#define INCLUDED_CALC_DATAVALUE
#endif
#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif


namespace calc {
  // DiskWrittenField declarations.
  class IOStrategy;
}



namespace calc {



//! A Field that is present on disk under a fileName, where load() will read
class DiskWrittenField : public DataValue
{

  const IOStrategy& d_ios;
  std::string       d_fileName;
  VS                d_vs;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  DiskWrittenField&           operator=           (DiskWrittenField const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DiskWrittenField               (DiskWrittenField const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DiskWrittenField               (const IOStrategy&  ios,
                                                   const std::string& fileName,
                                                   VS                 vs);

     virtual       ~DiskWrittenField              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  DataValue*        load                          ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual OVS       ovs                            () const;

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
