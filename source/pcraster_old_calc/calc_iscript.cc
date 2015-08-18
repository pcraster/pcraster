#include "stddefx.h"

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

//! dtor
calc::IScript::~IScript()
{
}

//! ctor
calc::IScript::IScript():
  d_timeStep(0),d_timerStart(0), d_timerEnd(1), d_timerSlice(0)
{
}


void calc::IScript::nextTimeStep()
{
  d_timeStep++;
}

bool calc::IScript::isDynamicModel() const
{ return d_timerSlice != 0; }

size_t calc::IScript::currentTimeStep() const
{
  return d_timeStep;
}

size_t calc::IScript::nrTimeSteps() const
{
  return d_timerEnd;
}


//! only calc::IndexTable does call this one
/*! it cheats on const-ness
 */
void calc::IScript::cmpToClone(const std::string& mapFileName) const
{
  calc::IScript *nc = const_cast<calc::IScript *>(this);
  nc->checkClone(mapFileName);
}
