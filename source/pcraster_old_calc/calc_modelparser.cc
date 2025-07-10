#include "stddefx.h"
#include "calc_modelparser.h"
#include "calc_parserinput.h"
#include "calc_script.h"



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

  int signalNr = 0;
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



