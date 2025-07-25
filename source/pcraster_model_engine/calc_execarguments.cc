#include "stddefx.h"
#include "calc_execarguments.h"
#include "calc_field.h"
#include "calc_runtimeenv.h"
#include "calc_domainerror.h"
#include "calc_operator.h"

/*!
  \file
  This file contains the implementation of the ExecArguments class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ExecArgumentsPrivate
{
public:

  ExecArgumentsPrivate()
  {
  }

  ~ExecArgumentsPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC EXECARGUMENTS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF EXECARGUMENTS MEMBERS
//------------------------------------------------------------------------------


/*
 * \param rte    the run time environment
 * \param nrFieldArgs the number of actual arguments used as input
 *
 * \todo
 *   maak dit abstract en creeer 2 sub-klassen:
 *   - ResultIsSrc  heeft srcDest()
 *   - "ANDERS"     heeft alle createResult()/createResults() in ctor?
 */
calc::ExecArguments::ExecArguments(
      const Operator& op,
      RunTimeEnv *rte,
      size_t nrActualInputs):
      d_op(op),
      d_rte(rte),
      d_fields(nrActualInputs-op.firstFieldInput()),
      d_result(op.nrResults(),nullptr),
      d_resultIsField(d_fields.size()), // init past a valid field
      d_firstNonFieldInput(nullptr)
{
  // reverse  reversed stack arguments
  for(size_t i=0;i<d_fields.size();++i)
    d_fields[d_fields.size()-i-1]= d_rte->popField();
  if (op.firstFieldInput())
   d_firstNonFieldInput=rte->popDataValue();
}

calc::ExecArguments::~ExecArguments()
{
#ifdef DEBUG_DEVELOP
  // result should be pushed and cleared in normal cases
  if (!std::uncaught_exceptions())
    for (auto & i : d_result)
      POSTCOND(!i);
#endif
  clean();
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::ExecArguments& calc::ExecArguments::operator=(const ExecArguments& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::ExecArguments::ExecArguments(const ExecArguments& rhs):
  Base(rhs)
{
}
*/

void  calc::ExecArguments::clean()
{
  for(size_t i=0; i < d_fields.size(); ++i)
    if (i != d_resultIsField && !d_doNotDelete.count(d_fields[i])) {
      deleteFromPcrme(d_fields[i]);
      d_doNotDelete.insert(d_fields[i]);
    }
  d_fields.clear();
  for (auto & i : d_result)
       deleteFromPcrme(i);
  deleteFromPcrme(d_firstNonFieldInput);
}

//! input fields (no non fields considered)
/*!
 *  taking the address is safe, reference to pointer
 */
const calc::Field& calc::ExecArguments::operator[](size_t a) const
{
  PRECOND(a < d_fields.size());
  return *d_fields[a];
}

size_t calc::ExecArguments::size() const
{
  return d_fields.size();
}

//! calculate result type on basis of actual types for result \a r
calc::DataType calc::ExecArguments::resultType(size_t r) const
{
  Operator::ArgTypes at;

  // first non field type are fixed
  //  get from operand description
  for(size_t i=0; i < d_op.firstFieldInput(); ++i)
    at.push_back(d_op.argType(i));

  // actual types of fields
  //  get from popped stack items
  for(auto d_field : d_fields)
    at.push_back(d_field->type());

  return d_op.computeResultType(at,r);
}

calc::Field& calc::ExecArguments::createResult()
{
  createResults();
  return *(d_result[0]);
}

void calc::ExecArguments::createResults()
{
  PRECOND(d_result.size()==d_op.nrResults());
  for(size_t i=0; i<d_result.size(); ++i) {
    PRECOND(!d_result[i]); // only call once
    d_result[i]=d_rte->createResultField(resultType(i));
  }
}

//! create a new destination
void* calc::ExecArguments::dest(size_t d)
{
  PRECOND(d < d_result.size());
  if (!d_result[d])
    createResults();
  return d_result[d]->dest();
}

//! use argument nr \a a as both read(src) and write destination
/*!
 *  \pre only one argument is used as read/write destination
 *       no createResult called
 */
void* calc::ExecArguments::srcDest(size_t a) {

  PRECOND(d_resultIsField >= d_fields.size());
  PRECOND(d_result.size() == 1);
  PRECOND(d_result[0]     == nullptr);

  // first get type, before removing a d_fields entry
  const DataType r(resultType(0));

  Field *f  = createDestCloneIfReadOnly(d_fields[a]);
  if (f != d_fields[a])
    deleteFromPcrme(d_fields[a]);
  d_fields[a]=f;

  d_result[0]=d_fields[a];
  d_resultIsField=a;
  d_result[0]->resetVs(r.vs());
  return d_result[0]->dest();
}

const void* calc::ExecArguments::src(size_t a) const
{
  PRECOND(a < d_fields.size());
  PRECOND(d_fields[a]);
  return d_fields[a]->src();
}

//! push all results on stack, check on MV
void calc::ExecArguments::pushResults()
{
  for(auto & i : d_result) {
   pushResult(i);
   d_doNotDelete.insert(i);
   i=nullptr;
  }
}

//! push \a result on stack, check on MV
void calc::ExecArguments::pushResult(Field *result)
{
  PRECOND(result);
  if (result->isMV()) {
    throw DomainError();
  }
  d_rte->pushField(result);
}

calc::Field& calc::ExecArguments::result(size_t r) const
{
  PRECOND(r < d_result.size());
  PRECOND(d_result[r]);
  return *d_result[r];
}

calc::DataValue* calc::ExecArguments::firstNonFieldInput() const
{
  return d_firstNonFieldInput;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


