#ifndef INCLUDED_CALC_P5STACK
#define INCLUDED_CALC_P5STACK

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTTESTFACTORY
#include "calc_asttestfactory.h"
#define INCLUDED_CALC_ASTTESTFACTORY
#endif
#ifndef INCLUDED_CALC_EXECUTOR
#include "calc_executor.h"
#define INCLUDED_CALC_EXECUTOR
#endif
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif

namespace calc {
  // P5Stack declarations.
}



namespace calc {

//! Class that will leave the results on the rte stack.
/*!
 * Historical this is an utility class to create Unit tests with the 5x5 maps.
 *
 * Executing with this class will have a default clone (inp5s.map).
 *
 * the upper left cell of the 5x5 maps is always a MV, others
 * are all equal, the map named encodes value and values scale map
 * by number and the letter of the VS;
 *   inp5stacks.map: scalar map with value 5
 *   inp1n.map: nominal map with value 1
 */
class  P5Stack : public ASTTestFactory {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  P5Stack&           operator=           (P5Stack const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   P5Stack               (P5Stack const& rhs);
                   P5Stack               ();

  std::auto_ptr<Executor>   d_e;
  std::auto_ptr<ASTScript>  d_as;
  bool                      d_keepLive;

  const Field* fieldCast    (const std::string& name) const;
  void         init         ();

public:
  struct CompileTest {
    std::string d_code;
    CompileTest(const std::string& code):
      d_code(code) {}
  };

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                                  P5Stack(const std::string&  codeOrId);
                                  P5Stack(const std::string&  codeOrId,
                                     bool  keepLastUse);
                                  P5Stack(CompileTest  code);

  /* virtual */    ~P5Stack              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool              contains        (const std::string& name) const;
  Field*            popResult       () const;

  static bool       equal           (const Field* f,
                                     double allValues,
                                     VS vs=VS_S,
                                     bool spatial=true);
         bool       equal           (const std::string& name,
                                     double allValues,
                                     VS vs=VS_S,
                                     bool spatial=true);

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
