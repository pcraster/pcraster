#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_COM_APPARGS
#include "com_appargs.h"
#define INCLUDED_COM_APPARGS
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//!  Constructor
com::AppArgs::AppArgs(const std::string& arg0, 
                      const std::string& otherArgs)
{
 POSTCOND(!arg0.empty());
 init(arg0+" "+otherArgs);
}

com::AppArgs::AppArgs (const std::string& allArgs)
{
 POSTCOND(!allArgs.empty());
 init(allArgs);
}

void com::AppArgs::init(const std::string& args)
{
  d_argv=0;
  d_buffer=0;
  try {
    std::vector<std::string>argv(com::split(args));
    POSTCOND(!argv.empty());
    d_argc   = argv.size();
    d_argv   = new char *[d_argc+1]; // +1 for end 0
    // args.size -> size of string
    //    d_argc -> nr of '\0' terminators
    d_buffer = new char  [args.size()+d_argc]; 
    char *ptr = d_buffer;
      for(size_t i=0; i < d_argc; i++) {
        d_argv[i] = ::strcpy(ptr,argv[i].c_str());
        ptr += argv[i].size()+1; // 1 for '\0'
      }
  } catch(...) {
   delete [] d_argv;
   delete [] d_buffer;
  }
  d_argv[d_argc]=0; // end 0
}

com::AppArgs::~AppArgs ()
{
  delete [] d_argv;
  delete [] d_buffer;
}

int com::AppArgs::argc() const
{
  return d_argc;
}

//! return the argv ptr-array
/*! sloppy non-constness of return type is historic
 * Note the argv() has size argc()+1 for terminating 0; argv()[argc()] == 0
 */
char **com::AppArgs::argv() const
{
  return d_argv;
}
