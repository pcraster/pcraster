#ifndef INCLUDED_CALC_CLIENTINTERFACE
#define INCLUDED_CALC_CLIENTINTERFACE



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



namespace calc {
  // ClientInterface declarations.
  class ASTScript;
  class IOStrategy;
  class Executor;
}



namespace calc {



//! services exposed by this dll
/*!
 * wrapped in pcrcalc.cc by calc::Pcrcalc
 *
 * Note: public method names are exactly as the names apearing in the C-api:
 *  e.g. pcr_ScriptXMLReflection instead of xmlReflection
 *
 * \todo
 *   implement calc::CmdLineCalc in terms of ClientInterface. First move
 *   calc::Calc into CmdLineCalc.
 */
class ClientInterface
{

  ASTScript*       d_script;
  //! used in the step wise functions
  Executor*        d_executor;

protected:
  std::string      d_scriptFileOrContents;
  bool             d_asFile;
  //! buffer filled in last call to xmlReflection();
  /*!
   * since we return a c_str()-ptr we must keep the buffer.
   */
  std::string      d_xmlReflectionBuffer;

  //! Assignment operator. NOT IMPLEMENTED.
  ClientInterface&           operator=           (ClientInterface const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ClientInterface               (ClientInterface const& rhs);

                   ClientInterface               ();

   void            load                          ();
   virtual ASTScript* createScriptAndAnalyzeNoContext()=0;
   void            clean                         ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ClientInterface               (const std::string& scriptFileOrContents,
                                                  bool               asFile);

  virtual         ~ClientInterface              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  const char*      pcr_ScriptXMLReflection       ();

  int              pcr_ScriptExecuteInitialStepMemory(void **data);
  int              pcr_ScriptExecuteNextTimeStepMemory(void **data);

  int              pcr_ScriptExecuteFinish       (void);

  void             pcr_ScriptExecute             ();

  void             pcr_ScriptReleaseAllAllocatedMemory();


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  ASTScript const& pcr_internalScript            () const;

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
