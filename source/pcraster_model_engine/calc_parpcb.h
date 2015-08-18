#ifndef INCLUDED_CALC_PARPCB
#define INCLUDED_CALC_PARPCB



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.



namespace geo {
  class RasterSpace;
}



namespace calc {

class ASTPar;
class Field;
class RunTimeEnv;

//! input/ouput Parameter of a PointCodeBlock::exec()
/*!
 * d_input and d_output's attributes (lastUse()) are significant!
 */
class ParPCB
{

  friend class ParPCBTest;

private:

  ParPCB&           operator=           (ParPCB const& rhs);
                   ParPCB               (ParPCB const& rhs);

  //! 0 if not input
  const ASTPar* d_input;
  //! 0 if not output
  const ASTPar* d_output;
  //! data
  Field*        d_field;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ParPCB               ();

  /* virtual */    ~ParPCB              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  Field*           releaseField        ();
  void             setInput            (const ASTPar* input);
  void             setOutput           (const ASTPar* output);
  void             setField            (Field* field);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const ASTPar*    input               () const;
  const ASTPar*    output              () const;
  Field*           field               () const;


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

void execPCB(
  std::vector<ParPCB*>& data,
  const void *dllFunctionAddress);


} // namespace calc

#endif
