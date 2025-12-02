#include "stddefx.h"
#include "calc_runtimeengine.h"
#include "calc_operator.h"
#include "calc_runtimeenv.h"
#include "calc_field.h"
#include "calc_datavalue.h"
#include "calc_datastorageid.h"
#include "calc_objectlink.h"

/*!
  \file
  This file contains the implementation of the RunTimeEngine class.
*/


//------------------------------------------------------------------------------

namespace calc
{

class PythonNone : public DataValue
{
public:
  OVS ovs() const override
  {
    return VS_NULL;
  }
};

static PythonNone pythonNone;

}  // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RUNTIMEENGINE MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF RUNTIMEENGINE MEMBERS
//------------------------------------------------------------------------------

calc::RunTimeEngine::RunTimeEngine(const geo::RasterSpace &rs) : d_rte(new RunTimeEnv(rs))
{
}

/* NOT IMPLEMENTED
//! Copy constructor.
calc::RunTimeEngine::RunTimeEngine(RunTimeEngine const& rhs)

  : Base(rhs)

{
}
*/


calc::RunTimeEngine::~RunTimeEngine()
{
  delete d_rte;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::RunTimeEngine& calc::RunTimeEngine::operator=(RunTimeEngine const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void calc::RunTimeEngine::pushUnmanaged(const DataValue *d)
{
  if (!d) {
    // exposed calls in PCRasterPython/PCRaster.cc are suspect to this
    d = &pythonNone;
  }
  auto *dv = const_cast<DataValue *>(d);
  dv->setPcrmeManaged(false);
  dv->setReadOnlyReference(true);
  d_rte->pushDataValue(dv);
}

//! pop a Field and transfer ownership to callee
/*!
 * callee can call delete on returned Field.
 */
calc::Field *calc::RunTimeEngine::releasePopField()
{
  Field *f = d_rte->popField();
  if (!f->pcrmeManaged()) {
    // f is created outside, and
    // pushed through pushUnmanaged

    // unset the original: f
    f->setReadOnlyReference(false);

    // return a copy
    // since RunTimeEngine garantuees to
    //  return newly created objects
    return f->createClone();
  }
  // f is created in PCRasterModelEngine

  // value must
  // be kept around, for later use
  if (f->readOnlyReference())
    f = f->createClone();
  f->setReadOnlyReference(false);
  f->setPcrmeManaged(false);
  return f;
}

//! pop an ObjectLink and transfer ownership to callee
/*!
 * callee can call delete on returned ObjectLink.
 */
calc::ObjectLink *calc::RunTimeEngine::releasePopObjectLink()
{
  auto *o = dynamic_cast<ObjectLink *>(d_rte->popDataValue());
  POSTCOND(!o->readOnlyReference());
  return o;
}

//! push \a d as read only value
/*!
 * see RunTimeEngine class documentation on the requirements of \a
 * d
 */
void calc::RunTimeEngine::pushObjectLink(const ObjectLink *d)
{
  pushUnmanaged(d);
}

//! push \a id as read only value
/*!
 * see RunTimeEngine class documentation on the requirements of \a
 * id 
 */
void calc::RunTimeEngine::pushDataStorageId(const DataStorageId *id)
{
  pushUnmanaged(id);
}

//! push \a f as read only value
/*!
 * see RunTimeEngine class documentation on the requirements of \a f
 */
void calc::RunTimeEngine::pushField(const Field *f)
{
  pushUnmanaged(f);
}

//! pushing with taking ownership of \a f
/*!
 * RunTimeEngine will delete f as some point.
 * \pre f is allocated with new()
 */
void calc::RunTimeEngine::transferPushField(Field *f)
{
  // push managed
  f->setPcrmeManaged(true);
  f->setReadOnlyReference(false);
  d_rte->pushField(f);
}

void calc::RunTimeEngine::setNrTimeSteps(size_t nrTimeSteps)
{
  Timer t = d_rte->timer();
  if (nrTimeSteps) {  // dynamic model
    t.setLastInt(nrTimeSteps);
    t.setStartInt(1);
  }
  d_rte->setTimer(t);
}

void calc::RunTimeEngine::setCurrentTimeStep(size_t currentTimeStep)
{
  Timer t = d_rte->timer();
  t.setCurrentInt(currentTimeStep);
  d_rte->setTimer(t);
  // std::cout << "timestep: " << currentTimeStep
  //           << "currentBPC:"<<Spatial::currentBPC()
  //           <<" maxBPC:" << Spatial::maxBPC()
  //           << std::endl;
}

/*!
 * \todo
 *   change op: Operator to std::string decod
      xml::Operation name/implName or ObjectLink::methodName
 */
void calc::RunTimeEngine::checkAndExec(const Operator &op, size_t nrPushedInputs)
{
  op.checkAndExec(d_rte, nrPushedInputs);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
