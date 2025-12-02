#include "stddefx.h"
#include "calc_iscript.h"

//! dtor
calc::IScript::~IScript()
{
}

//! ctor
calc::IScript::IScript()
{
}

void calc::IScript::nextTimeStep()
{
  d_timeStep++;
}

bool calc::IScript::isDynamicModel() const
{
  return d_timerSlice != 0;
}

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
void calc::IScript::cmpToClone(const std::string &mapFileName) const
{
  auto *nc = const_cast<calc::IScript *>(this);
  nc->checkClone(mapFileName);
}
