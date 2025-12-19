#ifndef INCLUDED_CALC_DATASTORAGEID
#define INCLUDED_CALC_DATASTORAGEID

#include "stddefx.h"
#include "pcraster_model_engine_export.h"
#include "calc_datavalue.h"

#include <string>


namespace calc {
  // DataStorageId declarations.
}



namespace calc {



//! Identification of DataStorage
/*!
   typically a filename
*/
class PCR_ME_EXPORT DataStorageId: public DataValue
{

  std::string      d_id;


private:

  //! Assignment operator. NOT IMPLEMENTED.
  DataStorageId&           operator=           (DataStorageId const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DataStorageId               (DataStorageId const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataStorageId               (const std::string& id);

  /* virtual */    ~DataStorageId              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void               setId                     (const std::string& id);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  OVS                ovs                       () const override;
  const std::string& id                        () const;

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
