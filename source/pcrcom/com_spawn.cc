#include "stddefx.h"
#include "com_appargs.h"
#include "com_spawn.h"

#include <sys/types.h>

#ifndef _MSC_VER
#include <unistd.h>
#else
#include <process.h>
#endif

#ifndef WIN32
#ifndef __APPLE__
#include <wait.h>
#endif
#include <spawn.h>
#endif

#include <cerrno>

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
  pid_t pid = 0;
  posix_spawnattr_t attr;
  if (posix_spawnattr_init(&attr) != 0) {
    _exit(-1);
  }
#ifndef __APPLE__
  if (posix_spawnattr_setflags(&attr, POSIX_SPAWN_USEVFORK) != 0) {
    _exit(-1);
  }
#endif
  int const result = posix_spawn(&pid, exeName, nullptr, &attr, (char *const *)args, nullptr);
  if (result != 0) {
    _exit(-1);
  }

  int status = 0;
  int const exitCode = waitpid(pid, &status, WUNTRACED);
  if (exitCode == -1) {
    //  std::cout << " execvp FAILURE REASON " << strerror(errno)
    //          << std::endl;
  }
  return status;
#endif
}

//! shorthand for spawn() with no args
int com::spawn(const char *exeName)
{
  const char *args[2] = {nullptr, nullptr};
  args[0] = exeName;
  return spawn(exeName, args);
}

//! args in one string to be chopped here
/*!
 * args with "-quotes or '-quotes to embed white space go wrong here
 */
int com::spawn(const std::string &exeName, const std::string &otherArgs)
{
  PRECOND(otherArgs.find("\"") == std::string::npos);
  PRECOND(otherArgs.find("\'") == std::string::npos);
  AppArgs const a(exeName, otherArgs);
  return spawn((const char *)a.argv()[0], (const char **)a.argv());
}
