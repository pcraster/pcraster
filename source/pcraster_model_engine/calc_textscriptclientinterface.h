#ifndef INCLUDED_CALC_TEXTSCRIPTCLIENTINTERFACE
#define INCLUDED_CALC_TEXTSCRIPTCLIENTINTERFACE

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_CLIENTINTERFACE
#include "calc_clientinterface.h"
#define INCLUDED_CALC_CLIENTINTERFACE
#endif


namespace calc {
  // TextScriptClientInterface declarations.
}



namespace calc {

class TextScriptClientInterface: public ClientInterface
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  TextScriptClientInterface&           operator=           (TextScriptClientInterface const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   TextScriptClientInterface               (TextScriptClientInterface const& rhs);

protected:
   ASTScript*      createScriptAndAnalyzeNoContext         ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TextScriptClientInterface(const std::string& scriptFileOrContents,
                                            bool asFile);

     virtual       ~TextScriptClientInterface              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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

#endif
