#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_SPAWN
#include "com_spawn.h"
#define INCLUDED_COM_SPAWN
#endif

// Library headers.

#include <sys/types.h>
#ifndef _MSC_VER
# include <unistd.h>
#else
# include <process.h>
#endif
#ifndef WIN32
  #ifndef __APPLE__
  #include <wait.h>
  #endif
#endif
#include <cerrno>

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_APPARGS
#include "com_appargs.h"
#define INCLUDED_COM_APPARGS
#endif



/*!
  \file
  This file contains the implementation of the Spawn class.
*/



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPAWN MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SPAWN MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//! Spawn a subprocess that blocks current execution
/*!
   \param exeName see execvp(3) or spawnvp(WIN32) for docs
   \param args  e see execvp(3) or spawnvp(WIN32) for docs

   \todo return value meaning (exit?) not clear

   \todo return exception with failure reason

   This spawn will call another application and waits until it is finished.
   For more sophisticated spawning see Qt's QProcess.

   Deprecated, NOT USED excep in calc_pointcodeblockdll.cc
*/
int com::spawn(const char *exeName, const char **args)
{
#ifdef WIN32
#ifdef _MSC_VER
  return _spawnvp(P_WAIT, (char *)exeName, (char **)args);
#else
  return ::spawnvp(P_WAIT, (char *)exeName, (char **)args);
#endif
#else
  int pid = vfork();
  if(pid == -1)
   return -1;
  if(!pid)
  {
    int exitCode=execvp((char *)exeName, (char **)args);
    if (exitCode==-1) {
      // std::cout << " execvp FAILURE REASON " << strerror(errno) 
      //        << std::endl;
    }
    _exit(-1);
  }

  int status;
  int exitCode = waitpid(pid, &status, WUNTRACED);
  if (exitCode==-1) {
    //  std::cout << " execvp FAILURE REASON " << strerror(errno) 
    //          << std::endl;
  }
  return status;
#endif
}

//! shorthand for spawn() with no args
int com::spawn(const char *exeName)
{
  const char *args[2] = { 0, 0 };
  args[0]=exeName;
  return spawn(exeName,args);
}

//! args in one string to be chopped here
/*!
 * args with "-quotes or '-quotes to embed white space go wrong here
 */
int com::spawn(const std::string& exeName,
               const std::string& otherArgs)
{
  PRECOND(otherArgs.find("\"") == std::string::npos);
  PRECOND(otherArgs.find("\'") == std::string::npos);
  AppArgs a(exeName,otherArgs);
  return spawn((const char *)a.argv()[0],(const char **)a.argv());
}
