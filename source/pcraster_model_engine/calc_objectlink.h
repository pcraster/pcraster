#ifndef INCLUDED_CALC_OBJECTLINK
#define INCLUDED_CALC_OBJECTLINK

#include "stddefx.h"
#include "pcraster_model_engine_export.h"
#include "calc_datavalue.h"

#include <vector>
#include <string>


namespace calc {
  // ObjectLink declarations.
  class ObjectLinkMeta;
  class RunTimeEnv;
  class Field;
}

// type "2" dispatch?
//  unknown/flexible nr of arguments?
#define OBJECTLINK_EXEC_DISPATCH(name)       \
      if(methodName == # name ) {            \
            d_obj->name(rte,nrFieldArgs);    \
            return true;                     \
      }



namespace calc {



//! external interface
class PCR_ME_EXPORT ObjectLink : public DataValue
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ObjectLink&           operator=           (ObjectLink const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ObjectLink               (ObjectLink const& rhs);

public:
  struct UnknownMethod {
  };

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ObjectLink               ();

           ~ObjectLink              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! type 1
  virtual void     exec1                    (const std::string&  methodName,
                                             const std::vector<Field *>& fields);

  //! type 2
  virtual void     exec2                    (const std::string&  methodName,
                                             RunTimeEnv*         rte,
                                             size_t              nrFieldArgs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  OVS               ovs                      () const override;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! type 1
inline void ObjectLink::exec1(const std::string&         ,
                             const std::vector<Field *>& )
{
}

//! type 2
inline void  ObjectLink::exec2(const std::string&  ,
                              RunTimeEnv*         ,
                              size_t              )
{}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
