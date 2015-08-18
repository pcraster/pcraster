#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_TUNE
#include "com_tune.h"
#define INCLUDED_COM_TUNE
#endif

// Library headers.
#ifndef __APPLE__
  #ifndef INCLUDED_MALLOC
  #include <malloc.h> // mallopt
  #define INCLUDED_MALLOC
  #endif
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Tune class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class TunePrivate
{
public:

  TunePrivate()
  {
  }

  ~TunePrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TUNE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TUNE MEMBERS
//------------------------------------------------------------------------------

com::Tune::Tune()
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
com::Tune::Tune(Tune const& rhs)

  : Base(rhs)

{
}
*/



com::Tune::~Tune()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
com::Tune& com::Tune::operator=(Tune const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

void com::tune() {
#ifdef __linux__
  // mingw does not recognize M_TOP_PAD
  // I guess mingw uses the Microsoft C runtime for malloc
#ifdef GCC
    // set it to 32 Mb
    #ifndef PCRASTER_LSB
      // mallopt does not work for LSB version
      mallopt(M_TOP_PAD,32*(1<<20));
    #endif
#else
// be aware that for example the intel compiler might do mallopt
// since it may use the standard libraries
#error find mallopt replacement for this build platform
#endif
#endif
}

/*
 * from googling   mosix  mallopt
 * Have a look at the new OReilly book 'High Performance Linux Cluster
 * with Rocks, Oscar and Mosix
#include <stdio.h>
#include <malloc.h>

static void mem_init_hook(void);
static void *mem_malloc_hook(size_t, const void *);
static void *(*glibc_malloc)(size_t, const void *);
void (*__malloc_initialize_hook)(void) = mem_init_hook;

static void mem_init_hook(void)
{
   com::tune();
   // mallopt (M_MMAP_MAX, 0);
}
*/
