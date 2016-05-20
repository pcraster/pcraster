#ifndef INCLUDED_CALC_OBJECTLINKMETA
#define INCLUDED_CALC_OBJECTLINKMETA

/*!
 * \file
 *   maps the ObjectLinkMeta xml tag
 */

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRDATATYPE
#include "pcrdatatype.h"
#define INCLUDED_PCRDATATYPE
#endif

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

// Module headers.
#ifndef INCLUDED_CALC_OBJECTLINKRUNTIME
#include "calc_objectlinkruntime.h"
#define INCLUDED_CALC_OBJECTLINKRUNTIME
#endif

namespace calc {
  // ObjectLinkMeta declarations.
  class RunTimeEnv;
}



namespace calc {

struct ObjectLinkMethod {
  //! name of method, empty if ctor
  /*!
   *  empty if ctor is Ok, as long as we only have
   *  one ctor
   */
  std::string                     d_name;
  //! results of method, empty if none
  std::vector<OP_ARGS> d_result;
  //! input/arguments of method, empty if none
  std::vector<OP_ARGS> d_input;

  ObjectLinkMethod(const std::string& name);
};

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning( disable:4251 )
#endif

//! describes interface from xml spec
class PCR_DLL_CLASS ObjectLinkMeta
{
public:
  typedef std::map<std::string, ObjectLinkMethod> MethodMap;

private:
  std::string          d_className;
  ObjectLinkFactoryPtr d_objectLinkFactory;



  //! key string equals ObjectLinkMethod::d_name
  MethodMap            d_methods;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ObjectLinkMeta               (const std::string& className,
                                                 const ObjectLinkFactoryPtr& objectLinkFactory);

  ObjectLinkMeta&           operator=           (ObjectLinkMeta const& rhs);

                   ObjectLinkMeta               (ObjectLinkMeta const& rhs);

  /* virtual */    ~ObjectLinkMeta              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             add                          (const std::string& methodName);
  void             pushBack                     (const std::string& methodName,
                                                 bool               result,
                                                 PCR_VS             vs,
                                                 PCR_ST             st);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const std::string&        className           () const;
  const MethodMap&          methodMap           () const;
  const ObjectLinkFactoryPtr&  objectLinkFactory   () const;

};
#ifdef _MSC_VER
#pragma warning (pop)
#endif



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
