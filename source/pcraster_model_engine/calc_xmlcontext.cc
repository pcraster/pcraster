#include "stddefx.h"
#include "calc_xmlcontext.h"
#include "PCRasterXSD.h"
#include "calc_areamap.h"
#include "calc_timer.h"

/*!
  \file
  This file contains the implementation of the XMLContext class.
*/


namespace calc
{

//------------------------------------------------------------------------------

/*
class XMLContextPrivate
{
public:

  XMLContextPrivate()
  {
  }

  ~XMLContextPrivate()
  {
  }

};
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC XMLCONTEXT MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF XMLCONTEXT MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

// pcrxml::CheckContext* createXMLCheckContext(AreaMap const& areaMap, Timer const& timer);

pcrxml::RunContext *createXMLRunContext(AreaMap const &areaMap, Timer const &timer)
{
  std::unique_ptr<pcrxml::CheckContext> cc(areaMap.createXMLContext());
  PRECOND(cc->areaMap().present());

  pcrxml::TimerContext t(timer.currentInt());
  t.start(timer.startInt());
  t.end(timer.lastInt());

  auto *rc(new pcrxml::RunContext(cc->areaMap().get(), t));

  if (cc->computationMask().present())
    rc->computationMask(cc->computationMask().get());

  return rc;
}


}  // namespace calc
