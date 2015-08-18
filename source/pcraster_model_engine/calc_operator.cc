#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"    // nrInSet
#define INCLUDED_CALC_VS
#endif
#ifndef INCLUDED_CALC_IOPIMPL
#include "calc_iopimpl.h"
#define INCLUDED_CALC_IOPIMPL
#endif
#ifndef INCLUDED_CALC_RTTYPECHECK
#include "calc_rttypecheck.h"
#define INCLUDED_CALC_RTTYPECHECK
#endif
#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif



/*!
  \file
  This file contains the implementation of the Operator class.
*/



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC OPERATOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF OPERATOR MEMBERS
//------------------------------------------------------------------------------

/* NOT IMPLEMENTED
//! Copy constructor.
calc::Operator::Operator(Operator const& rhs)

  : Base(rhs)

{
}
*/

//! ctor for built-in operators
calc::Operator::Operator(
    const char* name,
    const char *implName,
    MAJOR_CODE major,SYNTAX syntax,
    ExecType execType, IOpImpl* impl, bool commutative,size_t inputTailRepeat):
    d_name(name),
    d_implName(implName),
    d_inputTailRepeat(inputTailRepeat),
    d_major(major),
    d_syntax(syntax),
    d_pointOperator(false),
    d_domainIll(noDomainIll),
    d_execType(execType),
    d_impl(impl),
    d_objectLinkFactory(0),
    d_commutative(commutative)
{
  // PRECOND(d_impl); ALL IMPLEMENTED
}

//! ctor for external link-methods operators
calc::Operator::Operator(
    const std::string&           fullyQualifiedName,
    const std::string&           name,
    const std::vector<OP_ARGS>&  result,
    const std::vector<OP_ARGS>&  input,
    const ObjectLinkFactoryPtr&  objectLinkFactory):
    d_name(fullyQualifiedName),
    d_implName(name),
    d_inputTailRepeat(0),
    d_major(OP_ILL),
    d_syntax(SYNTAX_FUNC),
    d_pointOperator(false),
    d_domainIll(noDomainIll),
    d_execType(EXEC_TYPE_EXTERN),
    d_impl(0),
    d_objectLinkFactory(objectLinkFactory),
    d_commutative(false)
{
  for(size_t i=0; i < result.size(); ++i)
    pushBackResult(result[i].vs,result[i].st);
  for(size_t i=0; i < input.size(); ++i)
    pushBackInput(input[i].vs,input[i].st,false);
}

//! ctor for LinkInLibrary operators
calc::Operator::Operator(
    const std::string&           name,
    const std::vector<DataType>&  result,
    const std::vector<DataType>&  input):
    d_name(name),
    d_implName(name),
    d_inputs(input),
    d_inputTailRepeat(0),
    d_results(result),
    d_major(OP_ILL),
    d_syntax(SYNTAX_FUNC),
    d_pointOperator(false),
    d_domainIll(noDomainIll),
    d_execType(EXEC_TYPE_EXTERN),
    d_impl(0),
    d_objectLinkFactory(0),
    d_commutative(false)
{
}

calc::Operator::~Operator() {
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::Operator& calc::Operator::operator=(Operator const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void calc::Operator::pushBackInput(VS vs, ST st,bool repeat)
{
  if (repeat) {
    // can only be set once for the last argument
    PRECOND(d_inputTailRepeat==0);
    d_inputTailRepeat=1;
  }
  d_inputs.push_back(DataType(vs,st));
#ifdef DEBUG_DEVELOP
  // check if test in ExprVisitor are ok for non field
  if (d_inputs.size() == 1 &&  !isIn(vs,VS_FIELD)) {
    // first arg pushed and it is not a Field
    if (opCode() == OP_TIMEINPUT) {
      PRECOND(vs== VS_MAPSTACK);
    } else if (opCode() == OP_TIMEINPUTSPARSE) {
      PRECOND(vs== VS_MAPSTACK);
    } else if (opCode() == OP_TIMEINPUTMODULO) {
      PRECOND(vs== VS_MAPSTACK);
    } else if (opCode() == OP_LOOKUPMAPSTACK) {
      PRECOND(vs== VS_MAPSTACK);
    } else if (execType() == EXEC_TYPE_T_IN) {
      PRECOND(vs== VS_TSS);
    } else if (execType() ==  EXEC_TYPE_INDEX) {
      PRECOND(vs== VS_INDEXTABLE);
    } else if (execType() ==  EXEC_TYPE_EXTERN) {
      // VS_TABLE hack for StatTable Operator
      PRECOND(vs== VS_STRING || vs==VS_TABLE);
    } else if (execType() ==  EXEC_TYPE_Direct) {
      // FTTB
      PRECOND(vs== VS_TABLE);
    } else {
       switch(opCode()) {
        case OP_DYNWAVEFLUX:
        case OP_DYNWAVESTATE:
        case OP_DYNWAVE_MRF:
        case OP_LOOKUPSTATE:
        case OP_LOOKUPPOTENTIAL:
           PRECOND(vs== VS_TABLE);
           break;
        default:
          PRECOND(false);
       }
    }
  }
#endif
}

void calc::Operator::pushBackResult(VS vs, ST st)
{
  d_results.push_back(DataType(vs,st));
}

void calc::Operator::setPointOn(DomainIll domainType) {
    d_pointOperator=true;
    d_domainIll    =domainType;
}

const std::string& calc::Operator::name() const
{
  return d_name;
}

const std::string& calc::Operator::implName() const
{
  return d_implName;
}


/*!
 * \todo
 *  todo_runTimeMsgAccuMRF
 */
std::string calc::Operator::syntax() const
{
  // if then (else) apear as a function to the user
  if ( d_syntax == SYNTAX_FUNC ||
       d_major  == OP_IFTHEN  ||
       d_major  == OP_IFTHENELSE
     )
    return "function";
  // MRF FTTB -> todo_runTimeMsgAccuMRF
  POSTCOND(d_syntax == SYNTAX_OP || d_syntax == SYNTAX_MRF);
  return "operator";
}

size_t calc::Operator::firstFieldInput() const
{
  size_t firstFieldInput(0);
  for (size_t i=0; i < d_inputs.size(); ++i) {
    if (isIn(d_inputs[i].vs(),VS_FIELD))
      break;
    firstFieldInput++;
  }
  return firstFieldInput;
}


//! return type of argument nr \a argNr
calc::DataType calc::Operator::argType(size_t argNr) const
{
  return DataType(argVs(argNr),argSt(argNr));
}

//! return result type of result nr \a r (is set of possible types)
calc::DataType calc::Operator::resultType(size_t r) const
{
  PRECOND(r<d_results.size());

  return d_results[r];
}

size_t calc::Operator::nrResults() const
{
  return d_results.size();
}

//! compute result type \a r on base of the argument types (\a args)
calc::DataType calc::Operator::computeResultType(const ArgTypes& args,size_t r) const
{
  DataType ft(resultType(r));
  if (!ft.singleVs() && !firstFieldInput()) {
     // type derived from first arg with
     // multiple types in its arg definition
     // e.g. areaminimum
     size_t i = firstPolyInput();
     ft = DataType(args[i].vs(),ft.st());
  }
  if (ft.stEither()) {
    // derive spatial

    // Rule 1: is spatial if ONE OF the args is spatial
    for(size_t i=firstFieldInput(); i < args.size(); ++i)
      if(args[i].stSpatial())
        ft = DataType(ft.vs(),ST_SPATIAL);
    if (ft.stEither()) { // not set in Rule 1
      bool allNonSpatial=true;
      // Rule 2: non spatial if ALL the args are nonspatial
      for(size_t i=firstFieldInput(); i < args.size(); ++i)
        if(!args[i].stNonSpatial())
         allNonSpatial=false;
      if (allNonSpatial)
        ft = DataType(ft.vs(),ST_NONSPATIAL);
    }
    // still no yet known if no spatial arg and some args are unknown
  }
  return ft;
}

size_t calc::Operator::actualInput(size_t argNr) const
{
  if (argNr < d_inputs.size())
    return argNr;

  PRECOND(d_inputTailRepeat);

  size_t fixedPart       = d_inputs.size()-d_inputTailRepeat;
  size_t argPosInVarPart = (argNr-fixedPart) % d_inputTailRepeat;
  PRECOND(fixedPart+argPosInVarPart < d_inputs.size());
  return fixedPart+argPosInVarPart;
}


VS calc::Operator::argVs(size_t argNr) const {
  return d_inputs[actualInput(argNr)].vs();
}

ST calc::Operator::argSt(size_t argNr) const {
  return d_inputs[actualInput(argNr)].st();
}

//! an extraordinay high argument index
size_t calc::Operator::maxInput()
{
  return std::string::npos;
}

/*! returns argument position of the arguments that
    can have different valuescales but all must be equal,
    return Operator::maxInput if arguments vs do not have this
    restriction.
 */
size_t calc::Operator::polyEqualityInputBegin() const
{
  switch(d_execType) {
    case EXEC_TYPE_IFTHENELSE:
    case EXEC_TYPE_IFTHEN:
    case EXEC_TYPE_POLY:
             return firstPolyInput();
    default: return maxInput();
  }
}

//! returns first argument that can have multiple types
/*!
 * use in combo with isPolyInput:
 * \code
 * const Operator& op;
 * ASTExpr *e;
 * for(size_t i=op.firstPolyInput(); i<e->nrInputs()&&op.isPolyInput(i); ++i)
 *     ;
 * \endcode
 * easier setup then above, like begin/end-iterators, is difficult, due to
 * variable nr. of arguments
 */
int calc::Operator::firstPolyInput() const
{
  for(size_t i=firstFieldInput(); i < d_inputs.size(); i++)
    if (nrInSet(d_inputs[i].vs()) > 1)
      return i;
  POSTCOND(FALSE); // CW NEVER
  return 0;
}

//! does argument \a i accepts multiple types
/*!
 * \todo this stinks, see pcrcalc532
 */
bool calc::Operator::isPolyInput(size_t i) const
{
  // STINKS
 switch(d_major) {
   case OP_AREAMINIMUM:
   case OP_AREAMAXIMUM:
   case OP_AREAMAJORITY:
                return i < 1;
   default:
                return nrInSet(d_inputs[actualInput(i)].vs()) > 1;
 }
}

//! return an argument description
std::string calc::Operator::strInput(int nr) const
{
  std::ostringstream msg;
  // see todo_runTimeMsgAccuMRF
  //                            mrf like DynamicWave
  if (syntax() == "function" || d_inputs.size() > 2) {
    msg << "argument nr. "<<(nr+1);
    msg << " of function '";
  } else {
    switch(d_inputs.size()) {
     case 1:  msg << "operand";
             break;
     case 2: PRECOND(nr==0 || nr ==1);
        if (nr == 0)
           msg << "left operand";
        else
           msg << "right operand";
        break;
     default: POSTCOND(FALSE); // CW NEVER
    }
     msg << " of operator '";
  }
  msg << name() <<"'";
  return msg.str();
}

//! check correct nr of arguments
/*!
 * \param actualNrInputs number of args specified in invocation to check
 * \return empty string if OK, non-empty with message what's wrong
 */
std::string calc::Operator::checkNrInputs(size_t actualNrInputs) const
{
  // Check number of arguments
  int argCond = (int)d_inputs.size() - (int)actualNrInputs;
  int argTest = (!argCond) ? 0 : argCond/ABS(argCond);

  std::string msg;
  switch (argTest) {
   case 1:
     msg = "not enough arguments specified";
     break;
   case -1:
     if (d_inputTailRepeat)
       break;
     msg = "too many arguments specified"; //pcrcalc/test14
  }
  if (msg.empty())
    return msg;
  // pcrcalc/test25[23]
  return syntax()+" '"+name()+"' "+msg;
}

bool calc::Operator::isDynamicSectionOperation() const
{
   switch(opCode()) {
     case OP_TIMEINPUT:
     case OP_TIME:
     case OP_TIMESLICE:
            return true;
     default:
      return execType() == EXEC_TYPE_T_IN;
   }
}

/*!
 * \param rte RunTimeEnv
 * \param nrActualInputs number of arguments that are on the top of the rte stacks
 *        for this operation.
 */
void calc::Operator::exec(class RunTimeEnv* rte, size_t nrActualInputs)const
{
  try {
    if (d_impl)
     d_impl->exec(rte,*this,nrActualInputs);
    else {
      PRECOND(d_objectLinkFactory);
      if (nrResults() > 0 &&
          resultType(0).vs()==VS_OBJECT) {
        // ctor
        createObjectLink(*this,d_objectLinkFactory,"",rte,nrActualInputs);
      } else
        execObjectLinkMethod(*this,rte,nrActualInputs);
    }
  } catch(PosException& ) {
    throw;
  } catch(com::Exception& e) {
    // prefix runtime error with name of operation
    e.reset((boost::format("%1% %2%: %3%")
                % syntax() % name()
                % e.messages()).str());
    throw;
  }
}

/*! check and execute with arguments from rte stack
 * \param rte RunTimeEnv
 * \param nrActualInputs number of arguments that are on the top of the rte stacks
 *        for this operation.
 */
void calc::Operator::checkAndExec(
    RunTimeEnv* rte,
    size_t nrActualInputs) const
{
  rtTypeCheck(*this,rte, nrActualInputs);
  this->exec(rte,nrActualInputs);
}

bool calc::Operator::pointOperator() const
{
  return d_pointOperator;
}

//! return d_domainIll
calc::DomainIll calc::Operator::domainIll() const
{
  return d_domainIll;
}

MAJOR_CODE calc::Operator::opCode() const { return d_major; }
//! only still used in a few places
calc::ExecType calc::Operator::execType()   const { return d_execType; }

//! get value of d_commutative
bool calc::Operator::commutative() const
{
  return d_commutative;
}
//! result vs
VS         calc::Operator::vs()     const
{
  return d_results[0].vs();
}
//! result st
ST         calc::Operator::st()     const
{
  return d_results[0].st();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

