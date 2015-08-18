#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OBJECTLINKMETA
#include "calc_objectlinkmeta.h"
#define INCLUDED_CALC_OBJECTLINKMETA
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ObjectLinkMeta class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ObjectLinkMetaPrivate
{
public:

  ObjectLinkMetaPrivate()
  {
  }

  ~ObjectLinkMetaPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC OBJECTLINKMETA MEMBERS
//------------------------------------------------------------------------------


calc::ObjectLinkMethod::ObjectLinkMethod(const std::string& name):
   d_name(name)
{}

//------------------------------------------------------------------------------
// DEFINITION OF OBJECTLINKMETA MEMBERS
//------------------------------------------------------------------------------

calc::ObjectLinkMeta::ObjectLinkMeta(
    const std::string&       className,
    const ObjectLinkFactoryPtr& objectLinkFactory):
   d_className(className),
   d_objectLinkFactory(objectLinkFactory)
{
  // insert default ctor
  d_methods.insert(std::make_pair("",ObjectLinkMethod("")));
  // that will return an objectlink
  pushBack("",1,VS_OBJECT,ST_NON);
}



//! Copy constructor.
calc::ObjectLinkMeta::ObjectLinkMeta(ObjectLinkMeta const& rhs):
    d_className(rhs.d_className),
    d_objectLinkFactory(rhs.d_objectLinkFactory),
    d_methods(rhs.d_methods)
{
}



calc::ObjectLinkMeta::~ObjectLinkMeta()
{
}



//! Assignment operator.
calc::ObjectLinkMeta& calc::ObjectLinkMeta::operator=(ObjectLinkMeta const& rhs)
{
  if (this != &rhs) {
    d_className=rhs.d_className;
    d_objectLinkFactory=rhs.d_objectLinkFactory;
    d_methods=rhs.d_methods;
  }
  return *this;
}

void calc::ObjectLinkMeta::add(
    const std::string& methodName)
{
  if (!d_methods.count(methodName))
    d_methods.insert(std::make_pair(methodName,ObjectLinkMethod(methodName)));
}

void calc::ObjectLinkMeta::pushBack(
    const std::string& methodName,
    bool               result,
    PCR_VS             vs,
    PCR_ST             st)
{
  add(methodName);
  MethodMap::iterator pos=d_methods.find(methodName);
  POSTCOND(pos!=d_methods.end());
  OP_ARGS a = { vs, st};
  if (result)
    pos->second.d_result.push_back(a);
  else
    pos->second.d_input.push_back(a);
}

//! get value of d_className
const std::string& calc::ObjectLinkMeta::className() const
{
  return d_className;
}

//! return the MethodMap
const calc::ObjectLinkMeta::MethodMap&
 calc::ObjectLinkMeta::methodMap() const
{
  return d_methods;
}

//! get value of d_objectLinkFactory
const calc::ObjectLinkFactoryPtr& calc::ObjectLinkMeta::objectLinkFactory() const
{
  return d_objectLinkFactory;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



