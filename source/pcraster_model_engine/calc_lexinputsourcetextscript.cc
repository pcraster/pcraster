#include "stddefx.h"
#include "calc_lexinputsourcetextscript.h"
#include "com_file.h"

/*!
  \file
  This file contains the implementation of the LexInputSourceTextScript class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC LEXINPUTSOURCETEXTSCRIPT MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF LEXINPUTSOURCETEXTSCRIPT MEMBERS
//------------------------------------------------------------------------------

/*! ctor, opens the file containing the script (\a fileName)
 *  \throws com::Exception if \a fileName cannot be opened as an ASCII file
 */
calc::LexInputSourceTextScript::LexInputSourceTextScript(const com::PathName &fileName)
    : LexInputSource(fileName.toString())
{
  com::open(d_script, fileName);
}

calc::LexInputSourceTextScript::~LexInputSourceTextScript()
{
}

/*!
 * \todo
 *   hack for non-Ascii (cyrilic) must be upgraded, all LexInput
 *    interface must check/support other char sets.
 */
int calc::LexInputSourceTextScript::getChar()
{
  char c = 0;
  if (d_script.get(c)) {
    // calc::ParserTest::testNonAsciiScript()
    // 36:ERROR: not printable: FFFFFFF1(hex)
    if (c < 0)
      return ' ';
    return c;
  }
  return EOF;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
