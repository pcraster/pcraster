#include "stddefx.h"
#include "calc_modelerrortester.h"
#include "com_exception.h"
#include "calc_astscript.h"
#include "calc_stringparser.h"
#include "calc_messagestestdb.h"



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
     std::unique_ptr<ASTScript> const script(
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



