#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_POINTCODEBLOCKDLL
#include "calc_pointcodeblockdll.h"
#define INCLUDED_CALC_POINTCODEBLOCKDLL
#endif

// Library headers.
#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_SPAWN
#include "com_spawn.h"
#define INCLUDED_COM_SPAWN
#endif
#ifndef INCLUDED_COM_DYNAMICLIBRARY
#include "com_dynamiclibrary.h"
#define INCLUDED_COM_DYNAMICLIBRARY
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
// Module headers.
#ifndef INCLUDED_CALC_POINTCODEBLOCK
#include "calc_pointcodeblock.h"
#define INCLUDED_CALC_POINTCODEBLOCK
#endif



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


calc::PointCodeBlockDll::PointCodeBlockDll(const Blocks& l):
  d_dll(0)
{
  if (l.empty())
    return;
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
  if (d_dll)
    unload();
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

void calc::PointCodeBlockDll::load(const Blocks& l)
{
  d_dll=new com::DynamicLibrary("dlltest");
  for(size_t i=0; i < l.size(); ++i)
    l[i]->setDllFunctionAddress(d_dll->loadFunction(l[i]->dllFunctionName()));
}

void calc::PointCodeBlockDll::unload()
{
  delete d_dll;
  d_dll=0;
}

void calc::PointCodeBlockDll::generateSource(
       const Blocks& l) const
{
  std::ofstream s("dlltest.cc");

  s << "#ifndef INCLUDED_CALC_POINTCODEDLLHEADER" << std::endl
    << "#include \"calc_pointcodedllheader.h\"" << std::endl
    << "#define INCLUDED_CALC_POINTCODEDLLHEADER" << std::endl
    << "#endif" << std::endl;
  for(size_t i=0; i < l.size(); ++i)
   l[i]->genCode(s);
}

void calc::PointCodeBlockDll::compile() const
{
#ifdef WIN32
    throw com::Exception("PointCodeBlockDll compile not implemted for WIN32");
#else
 const std::string dll(
     "-o libdlltest.so -shared -rdynamic "\
     "-D_REENTRANT -fPIC -DPCR_DLL_TARGET ");
 const std::string asmS("-S ");
 const char *args=
      " -O3 -march=pentium4 -ffast-math -mfpmath=sse "\
      " -I../../../libs/PCRasterModelEngine   "\
      " -I../../../libs/pcrcom  "\
      " -I../../../libs/api     "\
      " -I../../../libs/app     "\
      " -I../../../libs/mathx   "\
      " -I../../../include      "\
      " dlltest.cc";
/*
 had -mcpu=i486 in make template
 -O3 -mfpmath=sse -march=pentium4
 " -g -DDEBUG -DDEBUG_DEVELOP -W -Wall -Wconversion -Wmissing-prototypes " \
# -DUNIX_FS -DCPU_LITTLE_ENDIAN \
*/
  int exitCode =com::spawn("gcc",dll+args);
  POSTCOND(!exitCode);
  if (exitCode) {
    throw com::Exception("compile failed");
  }
  exitCode =com::spawn("gcc",asmS+args);
  POSTCOND(!exitCode);
  if (exitCode) {
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



