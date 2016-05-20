#ifndef INCLUDED_CALC_XMLSCRIPTCLIENTINTERFACE
#define INCLUDED_CALC_XMLSCRIPTCLIENTINTERFACE

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_CLIENTINTERFACE
#include "calc_clientinterface.h"
#define INCLUDED_CALC_CLIENTINTERFACE
#endif


namespace pcrxml {
  class Script;
}



namespace calc {

class XMLScriptClientInterface: public ClientInterface
{

private:

  std::auto_ptr<pcrxml::Script> d_xml;

  //! Assignment operator. NOT IMPLEMENTED.
  XMLScriptClientInterface&           operator=           (XMLScriptClientInterface const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   XMLScriptClientInterface               (XMLScriptClientInterface const& rhs);

   void            parse                                  ();

protected:
   ASTScript*      createScriptAndAnalyzeNoContext        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   XMLScriptClientInterface(const std::string& scriptFileOrContents,
                                            bool asFile);

     virtual       ~XMLScriptClientInterface              ();

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
