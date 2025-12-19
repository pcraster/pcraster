#ifndef INCLUDED_CALC_XMLSCRIPTCLIENTINTERFACE
#define INCLUDED_CALC_XMLSCRIPTCLIENTINTERFACE

#include "stddefx.h"
#include "calc_clientinterface.h"

#include <string>
#include <memory>


namespace pcrxml {
  class Script;
}



namespace calc {

class XMLScriptClientInterface: public ClientInterface
{

private:

  std::unique_ptr<pcrxml::Script> d_xml;

  //! Assignment operator. NOT IMPLEMENTED.
  XMLScriptClientInterface&           operator=           (XMLScriptClientInterface const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   XMLScriptClientInterface               (XMLScriptClientInterface const& rhs);

   void            parse                                  ();

protected:
   ASTScript*      createScriptAndAnalyzeNoContext        () override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   XMLScriptClientInterface(const std::string& scriptFileOrContents,
                                            bool asFile);

           ~XMLScriptClientInterface              () override;

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
