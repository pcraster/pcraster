#ifndef INCLUDED_CALC_LEXINPUTSOURCETEXTSCRIPT
#define INCLUDED_CALC_LEXINPUTSOURCETEXTSCRIPT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_LEXINPUTSOURCE
#include "calc_lexinputsource.h"
#define INCLUDED_CALC_LEXINPUTSOURCE
#endif

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

  /* virtual */    ~LexInputSourceTextScript              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  int getChar();

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
