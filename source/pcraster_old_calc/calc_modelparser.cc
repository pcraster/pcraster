#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MODELPARSER
#include "calc_modelparser.h"
#define INCLUDED_CALC_MODELPARSER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_PARSERINPUT
#include "calc_parserinput.h"
#define INCLUDED_CALC_PARSERINPUT
#endif
#ifndef INCLUDED_CALC_SCRIPT
#include "calc_script.h"
#define INCLUDED_CALC_SCRIPT
#endif



/*!
  \file
  This file contains the implementation of the ModelParser class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MODELPARSER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MODELPARSER MEMBERS
//------------------------------------------------------------------------------

calc::ModelParser::ModelParser(ParserInput& pi,
                               Script& theScript):
   Parser(pi.tokenBuffer())
{
  initialize(theScript);

  int signalNr;
  model(&signalNr);
  script()->buildScript();
}

calc::ModelParser::~ModelParser()
{
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



