#ifndef INCLUDED_CALC_MODELERRORTESTER
#define INCLUDED_CALC_MODELERRORTESTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // ModelErrorTester declarations.
}



namespace calc {

class ASTScript;

//! execute a sample script and expect an error that is compared with messagestest.xml
class ModelErrorTester
{

private:

  const char       *d_id;

  //! Assignment operator. NOT IMPLEMENTED.
  ModelErrorTester&           operator=           (const ModelErrorTester& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ModelErrorTester               (const ModelErrorTester& rhs);

                   ModelErrorTester               ();

public:

  bool catched;
  bool msgEq;


  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    ModelErrorTester              (const char *id);


     virtual       ~ModelErrorTester              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void         loadAndExec();
  virtual void exec(ASTScript *script);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

// TesterClass derived from ModelErrorTester
#define MODEL_ERROR_TESTER(TesterClass,idX)  \
 TesterClass idX(# idX);                     \
 idX.loadAndExec();                          \
 BOOST_CHECK(idX.catched);                     \
 BOOST_CHECK(idX.msgEq);

#endif
