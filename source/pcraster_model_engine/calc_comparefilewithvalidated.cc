#include "stddefx.h"
#include "calc_comparefilewithvalidated.h"
#include "com_file.h"

#include <filesystem>

namespace fs = std::filesystem;



/*!
  \file
  This file contains the implementation of the CompareFileWithValidated class.
*/



namespace calc {

//------------------------------------------------------------------------------

/*
class CompareFileWithValidatedPrivate
{
public:

  CompareFileWithValidatedPrivate()
  {
  }

  ~CompareFileWithValidatedPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMPAREFILEWITHVALIDATED MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF COMPAREFILEWITHVALIDATED MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


//! compare contents of fileName with validated/fileName
/*!
 * only in support of unit tests
 *
 * a number of unit tests will write a file that must be validated
 * against the identical named file in the validated sub directory.
 *
 * \returns true if both files are existing and equal, false otherwise
 */
bool compareFileWithValidated(std::string const& fileName)
{
    fs::path computed(fileName);
    fs::path validated=fs::path("validated")/fileName;
    if(!com::filesExistsAndEqual(validated.string(),computed.string())) {
      std::string compareFileWithValidatedFailure=computed.string();
      PRINT_VAR(compareFileWithValidatedFailure);
      return false;
    }
    return true;
}


} // namespace calc

