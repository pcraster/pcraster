#ifndef INCLUDED_CALC_OBJECTLINK
#define INCLUDED_CALC_OBJECTLINK



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

// Module headers.
#ifndef INCLUDED_CALC_DATAVALUE
#include "calc_datavalue.h"
#define INCLUDED_CALC_DATAVALUE
#endif



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
class PCR_DLL_CLASS ObjectLink : public DataValue
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

     virtual       ~ObjectLink              ();

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
  OVS               ovs                      () const;

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
