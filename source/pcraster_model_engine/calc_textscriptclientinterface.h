#ifndef INCLUDED_CALC_TEXTSCRIPTCLIENTINTERFACE
#define INCLUDED_CALC_TEXTSCRIPTCLIENTINTERFACE

#include "stddefx.h"
#include "calc_clientinterface.h"

#include <string>


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
   ASTScript*      createScriptAndAnalyzeNoContext         () override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TextScriptClientInterface(const std::string& scriptFileOrContents,
                                            bool asFile);

           ~TextScriptClientInterface              () override;

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
