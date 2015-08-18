#ifndef INCLUDED_CALC_EXTSYM
#define INCLUDED_CALC_EXTSYM



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
  // ExtSym declarations.
}



namespace calc {

class  Position;
class  Symbol;


//! Symbol external to the script
/*!
 * A simple combination of a name (string) and a position
   for example symbol defined in a seperate bindings file, that must be added to
   the script environment.
*/
class ExtSym
{
private:
  std::string  d_name;
  Position    *d_pos;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  ExtSym&           operator=          (const ExtSym&);

                   ExtSym              (const ExtSym&);


                   ExtSym              (const std::string& name, const Position* pos);

                   ExtSym              (const std::string& name);

                   ExtSym              (const Symbol& s);

  /* virtual */    ~ExtSym             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const std::string& name() const {
    return d_name;
  }
  const Position* position() const;
  int positionPriority() const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------
extern bool operator<(const ExtSym& lhs, const ExtSym& rhs);
extern bool operator==(const ExtSym& lhs, const ExtSym& rhs);


//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
