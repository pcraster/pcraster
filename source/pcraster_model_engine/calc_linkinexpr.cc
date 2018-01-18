#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LINKINEXPR
#include "calc_linkinexpr.h"
#define INCLUDED_CALC_LINKINEXPR
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif

// Module headers.
#ifndef INCLUDED_CALC_EXECARGUMENTS
#include "calc_execarguments.h"
#define INCLUDED_CALC_EXECARGUMENTS
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_LINKINLIBRARY
#include "calc_linkinlibrary.h"
#define INCLUDED_CALC_LINKINLIBRARY
#endif
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_XMLDATATYPE
#include "calc_xmldatatype.h"
#define INCLUDED_CALC_XMLDATATYPE
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
/*!
  \file
  This file contains the implementation of the LinkInExpr class.
*/



namespace calc {

//------------------------------------------------------------------------------

template<typename TypeContainer>
  static std::vector<DataType> xml2DataType( TypeContainer const& c)
{
  std::vector<DataType> v;
  for(size_t i=0;i<c.size(); ++i) {
    v.push_back(xml2DataType(c[i]));
  }
  return v;
}

class OperatorCreator:
  public boost::noncopyable
{
  pcrxml::LinkInLibraryManifest const& d_manifest;


  const pcrxml::LinkInFunctionManifest* findFunction(std::string const& name) const {
    for(size_t i=0; i < d_manifest.function().size(); ++i) {
      if (name==d_manifest.function()[i].name())
       return &(d_manifest.function()[i]);
    }
    return 0;
  }

  const pcrxml::LinkInClassManifest* findClass(std::string const& name)  const{
    for(size_t i=0; i < d_manifest.class_().size(); ++i) {
      if (name==d_manifest.class_()[i].name())
       return &(d_manifest.class_()[i]);
    }
    return 0;
  }
  const pcrxml::LinkInClassMethod* findMethod(const pcrxml::LinkInClassManifest* c,
                                        std::string const& name) const
  {
    for(size_t i=0; i < c->method().size(); ++i) {
      if (name==c->method()[i].name())
       return &(c->method()[i]);
    }
    return 0;
  }

public:

  OperatorCreator(
   pcrxml::LinkInLibraryManifest const& manifest):
    d_manifest(manifest)
  {
  }

  ~OperatorCreator()
  {
  }

  Operator *createConstructor(std::string const& name) const {
   const pcrxml::LinkInClassManifest* c=findClass(name);
   if (!c)
      return 0;
   return new Operator(name,
       std::vector<DataType>(), // empty result
       xml2DataType(c->constructor().argument()));
  }

  Operator *createFunction(std::string const& name) const {

   std::vector<DataType> result;   // result types
   std::vector<DataType> argument; // input types

   const pcrxml::LinkInFunctionManifest *f=findFunction(name);
   if (!f)
     return 0;
   return new Operator(name,
                       xml2DataType(f->result()),
                       xml2DataType(f->argument()));
  }

  Operator         *createMethod(std::string const& className,
                                 std::string const& methodName) const
  {
    const pcrxml::LinkInClassManifest* c=findClass(className);
    if (!c)
      return 0;
    const pcrxml::LinkInClassMethod* m=findMethod(c,methodName);
    if (!m)
      return 0;
    return new Operator(className+"::"+methodName,
                        xml2DataType(m->result()),
                        xml2DataType(m->argument()));
  }
};



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LINKINEXPR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LINKINEXPR MEMBERS
//------------------------------------------------------------------------------

LinkInExpr::LinkInExpr(
         Id const& nameBefore,
         Id const& nameAfter,
         std::string const& stringArgument):
   BaseExpr(nameBefore.position(),nameBefore.name()+"::"+nameAfter.name()),
   d_nameBefore(nameBefore),
   d_nameAfter(nameAfter),
   d_stringArgument(stringArgument),
   d_library(0)
{
  // temporary no-op use shared_ptr::swap later
  d_op = boost::shared_ptr<Operator>(new Operator("0::0","0::0",
                      std::vector<OP_ARGS>(), // no results
                      std::vector<OP_ARGS>(), // no inputs
                      0));
}



/* DEFAULT
//! Copy constructor.
LinkInExpr::LinkInExpr(
         LinkInExpr const& rhs)

  : Base(rhs)

{
}
*/



LinkInExpr::~LinkInExpr()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
LinkInExpr& LinkInExpr::operator=(
         LinkInExpr const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/


ASTId* LinkInExpr::createClone()const
{
  return new LinkInExpr(*this);
}

const Operator&  LinkInExpr::op() const
{
  return *d_op;
}

/*! load library if not already loaded
    before this call a setAsMethod-call may identify
    that this is method invocation, otherwise it is
    a function or constructor
 */
void LinkInExpr::loadLibrary(const LinkInLibrary *library)
{
  if (d_library)
    return;

  d_library=library;
  OperatorCreator oc(d_library->manifest());

  if (isConstructor()) {
    d_op=boost::shared_ptr<Operator>(oc.createConstructor(d_className));
    if (!d_op.get())
      d_nameAfter.symError("unknown class");
  }
  if (isFunction()) {
    d_op=boost::shared_ptr<Operator>(oc.createFunction(d_functionName));
    if (!d_op.get())
      d_nameAfter.symError("unknown function");
  }
  if (isMethod()) {
    d_op=boost::shared_ptr<Operator>(oc.createMethod(d_className,d_methodName));
    if (!d_op.get()) {
      d_nameAfter.symError("unknown method");
    }
  }
  setNrReturns(d_op->nrResults());
}

void LinkInExpr::exec(RunTimeEnv* rte) const
{
  ExecArguments args(op(),rte,nrArgs());
  std::vector<void *> transferArray;

  pcrxml::LinkInExecuteInput li(context(*rte),callPoint());

  args.createResults();

  for (size_t i=0; i < nrReturns(); ++i) {
      li.result().push_back(toXMLFieldTypeOfValue(args.result(i).type()));
      transferArray.push_back(args.dest(i));
  }
  if (!d_stringArgument.empty())
    li.stringArgument(pcrxml::SpaceTrimmedToken(d_stringArgument));
  for (size_t i=0; i < nrArgs(); ++i) {
    li.argument().push_back(toXMLFieldTypeOfValue(args[i].type()));
    transferArray.push_back((void *)args.src(i));
  }
  PRECOND(d_library);

  if(transferArray.empty()){
    // linkin extension constructor without arguments
    d_library->execute(li, (void **)NULL);
  }
  else{
    d_library->execute(li,&(transferArray[0]));
  }

  args.pushResults();
}

void LinkInExpr::check()
{
  // empty context FTTB
  pcrxml::LinkInCheckInput in(pcrxml::CheckContext(),callPoint());

  for (size_t i=0; i < d_op->nrResults(); ++i)
      in.result().push_back(toXMLFieldType(d_op->resultType(i)));
  if (!d_stringArgument.empty())
    in.stringArgument(pcrxml::SpaceTrimmedToken(d_stringArgument));
  for (size_t i=0; i < nrArgs(); ++i)
    in.argument().push_back(toXMLFieldType(d_op->argType(i)));

  pcrxml::LinkInCheckResult r(d_library->check(in));
  if (r.error())
    posError(r.error().get());

  // reset operator to returned types
  std::vector<DataType> result=xml2DataType(r.result());
  std::vector<DataType> argument(xml2DataType(r.argument()));
  d_op = boost::shared_ptr<Operator>(new Operator(d_op->name(),result,argument));
}

//! get value of d_nameBefore
std::string const& LinkInExpr::nameBefore() const
{
  return d_nameBefore.name();
}

//! get value of d_nameAfter
std::string const& LinkInExpr::nameAfter() const
{
  return d_nameAfter.name();
}

void LinkInExpr::setAsMethod(std::string const&   className)
{
  d_className =className;
  d_objectName=nameBefore();
  d_methodName=nameAfter();
}

void LinkInExpr::setAsConstructor(ASTPar const&   objectName)
{
  d_objectPar  = boost::shared_ptr<ASTPar>(new ASTPar(objectName));
  d_objectName = objectName.name();
  d_libraryName= nameBefore();
  d_className  = nameAfter();
}

void LinkInExpr::setAsFunction()
{
  d_libraryName = nameBefore();
  d_functionName= nameAfter();
}

bool LinkInExpr::isMethod() const
{
  return !d_methodName.empty();
}

bool LinkInExpr::isFunction() const
{
  return !d_functionName.empty();
}

bool LinkInExpr::isConstructor() const
{
  return d_objectPar != 0;
}

const ASTPar* LinkInExpr::objectPar() const
{
  return d_objectPar.get();
}


pcrxml::RunContext LinkInExpr::context(RunTimeEnv const& rte) const
{
  std::shared_ptr<pcrxml::RunContext> rc(rte.createXMLContext());
  return *rc;
}

pcrxml::CallPoint  LinkInExpr::callPoint() const
{
  pcrxml::CallPoint cp;
  if (isFunction()) {
      // a function
      cp.function(pcrxml::Identifier(d_functionName));
      return cp;
  } else {
    // method or constructor
    cp.object(pcrxml::CallPoint::object_type(
              pcrxml::Identifier(d_className),   // className
              pcrxml::Identifier(d_objectName))); // objectName
    if (isConstructor()) {
       cp.object()->constructor(pcrxml::EmptyElement());
       return cp;
    }
    POSTCOND(isMethod());
    cp.object()->methodName(pcrxml::Identifier(d_methodName));
    return cp;
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc
