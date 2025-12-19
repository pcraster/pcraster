#ifndef INCLUDED_CALC_DISKWRITTENFIELD
#define INCLUDED_CALC_DISKWRITTENFIELD

#include "stddefx.h"
#include "calc_datavalue.h"
#include "calc_types.h"

#include <string>


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

           ~DiskWrittenField              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  DataValue*        load                          () override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  OVS       ovs                            () const override;

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
