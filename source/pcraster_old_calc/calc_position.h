#ifndef INCLUDED_CALC_POSITION
#define INCLUDED_CALC_POSITION



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h" // used often in combo with throwError
#define INCLUDED_CALC_QUOTE
#endif

namespace calc {
  // Position declarations.
}



namespace calc {



//! denotes a position of a symbol where it is found
/*!
   in the simple case is give a fileName, line nr and charachter position
*/
class Position
{

private:

  // Assignment operator. DEFAULT
  // Position&           operator=           (const Position&);

  int d_priority;

  //  Copy constructor. DEFAULT
  //              Position               (const Position&);
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   Position               ();

  virtual         ~Position               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setPriority(int priority);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void         throwError (const std::string& msg) const=0;
  void                 throwError (const std::ostringstream& msg) const;

  virtual Position*    createClone() const=0;
  //! determine priority for positional messages
  int                  priority()    const;
  virtual std::string  text       () const;

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
