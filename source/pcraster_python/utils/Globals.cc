#ifndef INCLUDED_PCRASTER_PYTHON_GLOBALS
#include "Globals.h"
#define INCLUDED_PCRASTER_PYTHON_GLOBALS
#endif

// External headers.

// Project headers.
#ifndef INCLUDED_DAL_RASTERDAL
#include "dal_RasterDal.h"
#define INCLUDED_DAL_RASTERDAL
#endif

#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h"
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif

#ifndef INCLUDED_CALC_RUNTIMEENGINE
#include "calc_runtimeengine.h"
#define INCLUDED_CALC_RUNTIMEENGINE
#endif

// Module headers.

#include "ppu_exception.h"


/*!
  \file
  This file contains the implementation of the Globals class.
*/

namespace {

} // Anonymous namespace



namespace pcraster {
namespace python {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GLOBALS MEMBERS
//------------------------------------------------------------------------------

Globals globals;

//------------------------------------------------------------------------------
// DEFINITION OF GLOBALS MEMBERS
//------------------------------------------------------------------------------

Globals::Globals()

  : dal::Client("", false, false),
    _rte(0),
    _rasterDal(0)

{
  init();
  _rasterDal = new dal::RasterDal(true);
}



Globals::~Globals()
{
  delete _rte;
  delete _rasterDal;
}



void Globals::init()
{
  calc::globalInit();      // Does dal::Library::initialise() too.
  _cloneSpace = geo::RasterSpace();
}



void Globals::setCloneSpace(
         geo::RasterSpace const& space)
{
  _cloneSpace = space;
  delete _rte;
  _rte = new calc::RunTimeEngine(space);
}



void Globals::setRandomSeed(
         unsigned int seed)
{
  // on Windows calling SetRan fails to
  // pick up the correct state table u from rand.c
  calc::setRan(seed);
}



geo::RasterSpace const& Globals::cloneSpace()
{
  return _cloneSpace;
}


calc::RunTimeEngine& Globals::rte()
{
  if(!_rte){
    throw PyUtilsException("no clone or area map specified, use setclone()");
  }
  return *_rte;
}



dal::RasterDal& Globals::rasterDal()
{
  assert(_rasterDal);
  return *_rasterDal;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace python
} // namespace pcraster

