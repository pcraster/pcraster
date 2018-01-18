#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_XMLCONTEXT
#include "calc_xmlcontext.h"
#define INCLUDED_CALC_XMLCONTEXT
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif

// Module headers.
#ifndef INCLUDED_CALC_AREAMAP
#include "calc_areamap.h"
#define INCLUDED_CALC_AREAMAP
#endif
#ifndef INCLUDED_CALC_TIMER
#include "calc_timer.h"
#define INCLUDED_CALC_TIMER
#endif


/*!
  \file
  This file contains the implementation of the XMLContext class.
*/



namespace calc {

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

pcrxml::RunContext*  createXMLRunContext  (AreaMap const& areaMap, Timer const& timer)
{
  std::unique_ptr<pcrxml::CheckContext> cc(areaMap.createXMLContext());
  PRECOND(cc->areaMap().present());

  pcrxml::TimerContext t(timer.currentInt());
  t.start(timer.startInt());
  t.end(timer.lastInt());

  pcrxml::RunContext* rc(new pcrxml::RunContext(cc->areaMap().get(),t));

  if (cc->computationMask().present())
    rc->computationMask(cc->computationMask().get());

  return rc;
}


} // namespace calc

