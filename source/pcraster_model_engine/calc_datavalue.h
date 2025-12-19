#ifndef INCLUDED_CALC_DATAVALUE
#define INCLUDED_CALC_DATAVALUE

#include "stddefx.h"
#include "pcraster_model_engine_export.h"
#include "calc_ovs.h"


namespace calc {
  // DataValue declarations.
}



namespace calc {


//! Base class for data values such as fields, and timeseries, mapstack, tables
/*!
 * DataValue has 2 fields to manage its manipulation within and outside PCRasterModelEngine:
 * d_readOnlyReference and d_pcrmeManaged. These fields must not be exposed to
 * external users (e.g. clients of RunTimeEngine and ClientInterface).
 *
 * Some sort of reference counted ptr might be an alternative to the scheme
 * implemented. Backdraw is that such a templated ptr aproach add complexity
 * when the objects are created outside.
 *
 * Within PCRasterModelEngine deletion of DataValue-based objects (e.g. Field, etc.) must
 * be deleted using deleteFromPcrme() or deleteAlways(). DVAutoPtr is meant as
 * replacement of std::auto_ptr<>  for DataValue-based objects.
 */
class PCR_ME_EXPORT DataValue
{

protected:

  // Assignment operator. DEFAULT
  // DataValue&           operator=           (const DataValue& rhs);

  //  Copy constructor. DEFAULT
  //               DataValue               (const DataValue& rhs);

                   DataValue               ();

  /*!
   * true if this object can not be modified nor deleted since its value
   * may be needed later, false otherwise.
   */
  bool             d_readOnlyReference{false};
  /*!
   * true if PCRasterModelEngine can delete, false otherwise.
   */
  bool             d_pcrmeManaged{true};

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


     virtual      ~DataValue                ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  virtual DataValue* load                     ();
  void               setReadOnlyReference     (bool readOnlyReference);
  void               setPcrmeManaged          (bool pcrmeManaged);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual OVS      ovs                        () const=0;
  virtual bool     readOnlyReference          () const;
  bool             pcrmeManaged               () const;

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

void deleteFromPcrme    (const DataValue *dv);
void deleteAlways       (DataValue *dv);


} // namespace calc

#endif
