#ifndef INCLUDED_CALC_LEXINPUTSOURCESTRING
#define INCLUDED_CALC_LEXINPUTSOURCESTRING

#include "stddefx.h"
#include "calc_lexinputsource.h"

#include <string>


namespace calc {
  // LexInputSourceString declarations.
}



namespace calc {



/*!
 * \brief get input from a text string
 * note that LexInputSourceString::getChar() will return an additional '\\n'
 * at the end of the input (before EOF) that is not present in \a d_argv
 * \todo
 *  kan nu met apart Position deriviaat zoiet maken als:
 *  <pre>
 *    cmd line argument
 *      ^
 *    daar zit de fout
 *  </pre>
 */
class LexInputSourceString : public LexInputSource {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  LexInputSourceString&           operator=           (const LexInputSourceString&);

  //! Copy constructor. NOT IMPLEMENTED.
                   LexInputSourceString               (const LexInputSourceString&);

  //! eof found
  bool    d_eof{};

  size_t  d_ptr{0};

  std::string d_contents;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  LexInputSourceString                                (const std::string& contents);

  /* virtual */    ~LexInputSourceString              () override;

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
