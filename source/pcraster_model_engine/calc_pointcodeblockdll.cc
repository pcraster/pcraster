#include "stddefx.h"
#include "calc_pointcodeblockdll.h"
#include "com_spawn.h"
#include "com_dynamiclibrary.h"
#include "com_exception.h"
#include "calc_pointcodeblock.h"

#include <fstream>

/*!
  \file
  This file contains the implementation of the PointCodeBlockDll class.
*/


//------------------------------------------------------------------------------

/*
namespace calc {

class PointCodeBlockDllPrivate
{
public:

  PointCodeBlockDllPrivate()
  {
  }

  ~PointCodeBlockDllPrivate()
  {
  }

};

} // namespace calc
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC POINTCODEBLOCKDLL MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF POINTCODEBLOCKDLL MEMBERS
//------------------------------------------------------------------------------


calc::PointCodeBlockDll::PointCodeBlockDll(const Blocks &l)
{
  if (l.empty()) {
    return;
  }
  generateSource(l);
  compile();
  // com::remove("dlltest.cc");
  load(l);
}

/* NOT IMPLEMENTED
//! Copy constructor.
calc::PointCodeBlockDll::PointCodeBlockDll(PointCodeBlockDll const& rhs)

  : Base(rhs)

{
}
*/


calc::PointCodeBlockDll::~PointCodeBlockDll()
{
  if (d_dll != nullptr) {
    unload();
  }
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::PointCodeBlockDll& calc::PointCodeBlockDll::operator=(PointCodeBlockDll const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void calc::PointCodeBlockDll::load(const Blocks &l)
{
  d_dll = new com::DynamicLibrary("dlltest");
  for (auto i : l) {
    i->setDllFunctionAddress(d_dll->loadFunction(i->dllFunctionName()));
  }
}

void calc::PointCodeBlockDll::unload()
{
  delete d_dll;
  d_dll = nullptr;
}

void calc::PointCodeBlockDll::generateSource(const Blocks &l) const
{
  std::ofstream s("dlltest.cc");

  s << "#ifndef INCLUDED_CALC_POINTCODEDLLHEADER" << '\n'
    << "#include \"calc_pointcodedllheader.h\"" << '\n'
    << "#define INCLUDED_CALC_POINTCODEDLLHEADER" << '\n'
    << "#endif" << '\n';
  for (auto i : l) {
    i->genCode(s);
  }
}

void calc::PointCodeBlockDll::compile() const
{
#ifdef WIN32
  throw com::Exception("PointCodeBlockDll compile not implemted for WIN32");
#else
  const std::string dll("-o libdlltest.so -shared -rdynamic "
                        "-D_REENTRANT -fPIC -DPCR_DLL_TARGET ");
  const std::string asmS("-S ");
  const char *args = " -O3 -march=pentium4 -ffast-math -mfpmath=sse "
                     " -I../../../libs/PCRasterModelEngine   "
                     " -I../../../libs/pcrcom  "
                     " -I../../../libs/api     "
                     " -I../../../libs/app     "
                     " -I../../../libs/mathx   "
                     " -I../../../include      "
                     " dlltest.cc";
  /*
 had -mcpu=i486 in make template
 -O3 -mfpmath=sse -march=pentium4
 " -g -DDEBUG -DDEBUG_DEVELOP -W -Wall -Wconversion -Wmissing-prototypes " \
# -DUNIX_FS -DCPU_LITTLE_ENDIAN \
*/
  int exitCode = com::spawn("gcc", dll + args);
  POSTCOND(!exitCode);
  if (exitCode != 0) {
    throw com::Exception("compile failed");
  }
  exitCode = com::spawn("gcc", asmS + args);
  POSTCOND(!exitCode);
  if (exitCode != 0) {
    throw com::Exception(".s failed");
  }
#endif
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
