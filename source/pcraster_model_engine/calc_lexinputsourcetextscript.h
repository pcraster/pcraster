#ifndef INCLUDED_CALC_LEXINPUTSOURCETEXTSCRIPT
#define INCLUDED_CALC_LEXINPUTSOURCETEXTSCRIPT

#include "stddefx.h"
#include "calc_lexinputsource.h"

#include <fstream>


namespace com {
  class PathName;
}

namespace calc {

class PositionText;

//! get input from normal ascii text script
class LexInputSourceTextScript : public LexInputSource {
private:


  //! Assignment operator. NOT IMPLEMENTED.
  LexInputSourceTextScript&           operator=           (const LexInputSourceTextScript&);

  //! Copy constructor. NOT IMPLEMENTED.
                   LexInputSourceTextScript               (const LexInputSourceTextScript&);

  std::ifstream d_script;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  //! Install script given as option (-f)
  /*!
   *  \throws com::Exception if fileName cannot be opened as an ASCII file
   */
  LexInputSourceTextScript(const com::PathName& file);

  /* virtual */    ~LexInputSourceTextScript              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  int getChar() override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
//  PositionText* createPositionText(size_t lineNr, size_t charNr)const;

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
