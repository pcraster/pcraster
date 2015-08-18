#include "stddefx.h"

#ifndef INCLUDED_CALC_FILE
#include "calc_file.h"
#define INCLUDED_CALC_FILE
#endif

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

bool calc::File::d_testCaseTypeOnExistingName=false;

//! check if output name is valid
/*! \exception com::OpenFileError with description of problem
 */
void calc::File::validateOutputName() const
{
   com::PathInfo pi(d_name);
   pi.testValidName();
   pi.testOpenForWriting();
}

//! check if name is valid, and the file exists
/*! \exception com::OpenFileError with description of problem
 * \bug
 *   if -c generates error then Pos is the invocation not the
 *   point of the binding.
 */
void calc::File::validateExisting() const
{
   com::PathInfo pi(d_name);
   if (d_testCaseTypeOnExistingName)
      pi.testCaseSensitiveName();
   else
      pi.testOpenForReading();
}
