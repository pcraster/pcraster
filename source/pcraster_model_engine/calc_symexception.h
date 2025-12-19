#ifndef INCLUDED_CALC_SYMEXCEPTION
#define INCLUDED_CALC_SYMEXCEPTION

#include "stddefx.h"
#include "calc_posexception.h"


namespace calc {
  // SymException declarations.
}



namespace calc {

class Position;
class ASTPar;

//! Error message on a certain symbol with a position
/*!
 * The idea is that a SymException can be rewritten if there
 * is additonal info on symbolName, for example in ASTSymbolTable, where
 * the binding to the symbol lives. If not catched, it is handled by default
 * as a PosException (or com::Exception).
 */
class SymException : public PosException
{

private:

  //  Assignment operator. DEFAULT
  // SymException&           operator=           (const SymException& rhs);

  //  Copy constructor.    DEFAULT
  //               SymException               (const SymException& rhs);

  std::string      d_position;
  std::string      d_symbolName;
  std::string      d_message;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SymException               (
                       const Position&    pos,
                       const std::string& symbolName,
                       const std::string& message);

                   SymException               (
                       const ASTPar*      par,
                       const std::string& message);

  /* virtual */    ~SymException              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const std::string& symbolName                () const;
  void               throwPos                  (const std::string& symbolInfo) const;

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
