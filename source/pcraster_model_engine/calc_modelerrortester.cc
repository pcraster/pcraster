#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MODELERRORTESTER
#include "calc_modelerrortester.h"
#define INCLUDED_CALC_MODELERRORTESTER
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
#ifndef INCLUDED_CALC_STRINGPARSER
#include "calc_stringparser.h"
#define INCLUDED_CALC_STRINGPARSER
#endif
#ifndef INCLUDED_CALC_MESSAGESTESTDB
#include "calc_messagestestdb.h"
#define INCLUDED_CALC_MESSAGESTESTDB
#endif



/*!
  \file
  This file contains the implementation of the ModelErrorTester class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ModelErrorTesterPrivate
{
public:

  ModelErrorTesterPrivate()
  {
  }

  ~ModelErrorTesterPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MODELERRORTESTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MODELERRORTESTER MEMBERS
//------------------------------------------------------------------------------

calc::ModelErrorTester::ModelErrorTester(const char *id):
  d_id(id)
{
}

void calc::ModelErrorTester::loadAndExec()
{
   catched=false;
   msgEq=false;
   try {
     std::auto_ptr<ASTScript> script(
     StringParser::createScript(MessagesTestDB::instance()->model(d_id)));
     exec(script.get());
   } catch (const com::Exception& s) {
    msgEq=MessagesTestDB::instance()->equals(d_id,s,"");
    catched=true;
   }
}

calc::ModelErrorTester::ModelErrorTester()
{
}

calc::ModelErrorTester::~ModelErrorTester()
{
}


/* NOT IMPLEMENTED
//! Assignment operator.
calc::ModelErrorTester& calc::ModelErrorTester::operator=(const ModelErrorTester& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::ModelErrorTester::ModelErrorTester(const ModelErrorTester& rhs):
  Base(rhs)
{
}
*/

void calc::ModelErrorTester::exec(ASTScript *)
{
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



