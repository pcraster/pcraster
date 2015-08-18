#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_STACKINPUT
#include "calc_stackinput.h"
#define INCLUDED_CALC_STACKINPUT
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif
#ifndef INCLUDED_COM_CLONE
#include "com_clone.h"
#define INCLUDED_COM_CLONE
#endif
// Module headers.
#ifndef INCLUDED_CALC_IOSTRATEGY
#include "calc_iostrategy.h"
#define INCLUDED_CALC_IOSTRATEGY
#endif
#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#include "calc_iofieldstrategy.h"
#define INCLUDED_CALC_IOFIELDSTRATEGY
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif
#ifndef INCLUDED_CALC_TIMER
#include "calc_timer.h"
#define INCLUDED_CALC_TIMER
#endif
#ifndef INCLUDED_CALC_MAPSTACKTYPE
#include "calc_mapstacktype.h"
#define INCLUDED_CALC_MAPSTACKTYPE
#endif
#ifndef INCLUDED_CALC_DOMAINERROR
#include "calc_domainerror.h"
#define INCLUDED_CALC_DOMAINERROR
#endif



/*!
  \file
  This file contains the implementation of the StackInput class.
*/



//------------------------------------------------------------------------------




//------------------------------------------------------------------------------
// DEFINITION OF STATIC STACKINPUT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STACKINPUT MEMBERS
//------------------------------------------------------------------------------

calc::StackInput::StackInput(
    IOStrategy&         fios,
    const std::string&  stackName,
    const MapStackType& type):
  d_stackName(stackName),
  d_fios(fios),
  d_fs(fios.ioFieldStrategy()),
  d_itemToLoad(fios.timer().lastInt()+1,0),
  d_type(VS_FIELD,ST_SPATIAL),
  d_cachedField(0),
  d_cachedTimeStep(0)
{
  // refactor this stuff
  //  ctor to initItemToLoad
  //   Lookup up out of here (not time related, no caching)

  if (type.use()== MapStackType::Lookup) {
    // find only nr 1 must exist
    checkMap(1);
    return;
  }


  PRECOND(type.use()!=MapStackType::Unknown);
  // check if for each timestep a map is there with identical VS
  // and if they ALL have the same format supported by d_fios.ioFieldStrategy();
  // esrigrid/test26
  d_itemToLoad[0]=0; // dummy for fall back to previous
  size_t firstExisting=0;

  for(size_t i=fios.timer().startInt(); i < d_itemToLoad.size(); i++) {
     // default for Full, ok for Sparse if existing
     d_itemToLoad[i]=i;
     switch(type.use()) {
      case MapStackType::Modulo: {
         // example highestTimestepAvailable==3
         // i = 1 2 3 4 5 6 7
         //%3 = 1 2 0 1 2 0 1
         // m = 1 2 3 1 2 3 1
         size_t m = i %  type.highestTimestepAvailable();
         m = (m == 0) ?  type.highestTimestepAvailable() : m;
         d_itemToLoad[i]= m;
       }
       break;
      case MapStackType::Sparse:
        // fall back to previous
       if (!exists(i))
          d_itemToLoad[i]=d_itemToLoad[i-1];
       break;
       default:;
     }
     if (d_itemToLoad[i]!=i)
       continue; // do not check non existing item

    // item i expected exist, at this point in code
    // but may cause a file not found message
    if (!firstExisting)
     firstExisting=i;

    checkMap(i);

  } // eofor
  // at least 1 element needed
  if (!firstExisting) {
       // pcrcalc376
       throw com::Exception(" on checking map-stack:\n not a single map found");
  }
  // if firstExisting is not the d_fios.timer().startInt() one
  //  set first ones to firstExisting
  for(size_t i=d_fios.timer().startInt(); i < firstExisting; ++i)
    d_itemToLoad[i]=firstExisting;
}

calc::StackInput::~StackInput()
{
  delete d_cachedField;
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::StackInput& calc::StackInput::operator=(const StackInput& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! Copy constructor.
calc::StackInput::StackInput(const StackInput& rhs):
  DataValue(rhs),
  d_stackName      (rhs.d_stackName),
  d_fios           (rhs.d_fios),
  d_fs             (d_fios.ioFieldStrategy()),
  d_itemToLoad     (rhs.d_itemToLoad),
  d_type           (rhs.d_type),
  d_cachedField    (com::non0Clone(rhs.d_cachedField)),
  d_cachedTimeStep (rhs.d_cachedTimeStep)
{
}

VS calc::StackInput::fieldReturnVs() const
{
  return d_type.vs();
}

calc::OVS calc::StackInput::ovs() const
{
  return VS_MAPSTACK;
}

//! read values for timestep t in buffer
/*!
 * \todo now always a copy, is not needed
 */
calc::Field *calc::StackInput::read(
    size_t t)
{
  if (!t)
     throw DomainError("timeinput outside dynamic section");
/* TODO now checked in resolve, but other runtime engines might need this
  if (rowIndex >= stack->nrSteps())
     throw DomainError("map-stack too short");
*/
  PRECOND(t < d_itemToLoad.size());
  // i is read as the value for t
  size_t i= d_itemToLoad[t];
  if (i != d_cachedTimeStep) {
    delete d_cachedField;
    d_cachedField = readField(i);
    d_cachedTimeStep=i;
  }
  return d_cachedField->createClone();
}

//! read values for mapstack item \a t in buffer
calc::Field *calc::StackInput::readLookup(
    int t)
{
  if (t < 0)
   throw DomainError("index must be > 0");
  checkMap(t);
  return readField(t);
}

calc::Field *calc::StackInput::readField(size_t i) const
{
  return d_fios.createReadField(name(i),DataType(fieldReturnVs(),ST_SPATIAL));
}

//! return name of stack item for timestep t
std::string calc::StackInput::name(size_t t) const
{
  return d_fs.makeStackItemName(d_stackName,t);
}

VS calc::StackInput::checkMap(size_t i) 
{
    VS vs=VS_FIELD;
    try {
     d_fs.checkInputMap(vs,name(i));
     try {
       d_type.restrict(DataType(vs,ST_SPATIAL));
     } catch (const VSClash& ) { // pcrcalc250(ac)
      std::ostringstream s;
      s << " does not have the same data type as first element ("
        << d_type.vs() <<")";
      throw com::Exception(s.str());
     }
    } catch ( com::Exception& msg ) {
      // rewrote construction of posError param. win32/release
      //  gives strange error
      std::ostringstream s;
      s << " on checking map-stack:\n  element "
        << name(i) << ": " << msg.messages();
       // pcrcalc344(a)
       throw com::Exception(s.str());
    }
    return vs;
}


//! is there a file for timestep t
/*!
 * checks only for existence of the file (or directory), later
 * a check is done if it is a valid  stack item
 */
bool calc::StackInput::exists(size_t t) const
{
  return com::pathExists(name(t));
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



