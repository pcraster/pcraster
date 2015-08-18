#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_CPUCYCLECOUNTER
#include "com_cpucyclecounter.h"
#define INCLUDED_COM_CPUCYCLECOUNTER
#endif

// Library headers.
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the CpuCycleCounter class.
*/



//------------------------------------------------------------------------------

// using the Pentium RDTSC instruction
static com::CpuCycleCounter::Uint64 pentiumClock(void)
{
  typedef com::CpuCycleCounter::Uint64 UI64;
  PRECOND(sizeof(UI64) == 8);
#ifdef GCC
  unsigned long high, low;
   __asm__ __volatile__(".byte 0x0f,0x31" : "=a" (low), "=d" (high));
   return ((UI64) high << 32) + low;
#else
   return 0;
#endif
}

com::CpuCycleCounter::Interval::Interval():
    d_total(0),
    d_start(0)
{
  PRECOND(sizeof(Uint64) == 8);
}


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CPUCYCLECOUNTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CPUCYCLECOUNTER MEMBERS
//------------------------------------------------------------------------------

com::CpuCycleCounter::CpuCycleCounter(size_t nrCounters):
   d_counters(nrCounters)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
com::CpuCycleCounter::CpuCycleCounter(CpuCycleCounter const& rhs)

  : Base(rhs)

{
}
*/



com::CpuCycleCounter::~CpuCycleCounter()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
com::CpuCycleCounter& com::CpuCycleCounter::operator=(CpuCycleCounter const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void com::CpuCycleCounter::start(size_t nr)
{
  PRECOND(nr < d_counters.size());
  d_counters[nr].d_start=pentiumClock();
}

void com::CpuCycleCounter::stop(size_t nr)
{
  PRECOND(nr < d_counters.size());
  d_counters[nr].d_total+=
    pentiumClock()-d_counters[nr].d_start;
  d_counters[nr].d_start=0;
}

/*
 * \todo
 *   automatic determine nr of bits to print something
 *    between 1E5 en 1e4
 */
// void com::CpuCycleCounter::print() const
// {
//   Uint64 max=0;
//   for(size_t i=0;i<d_counters.size();++i)
//     com::maximize(max,d_counters[i].d_total);
// 
//   size_t skipBits=0;
//   while(max > 10000) {
//     max = max >> 1;
//     skipBits++;
//   }
// 
//   std::cout << "skip bits: " << skipBits  << std::endl;
// 
//   for(size_t i=0;i<d_counters.size();++i)
//    std::cout << "counter " << i << " : "
//              << (d_counters[i].d_total >> skipBits) << std::endl;
// }

/* return vector with totals shifted into the 32 bit realm
 * \param maxCount  maximum value possible returned
 *
 * all values are bit shifted until all value are smaller than
 * \a maxCount
 */
std::vector<size_t>  com::CpuCycleCounter::counters(
    size_t maxCount) const
{
  Uint64 max=0;
  for(size_t i=0;i<d_counters.size();++i)
    com::maximize(max,d_counters[i].d_total);

  size_t skipBits=0;
  while(max > maxCount) {
    max = max >> 1;
    skipBits++;
  }

  std::vector<size_t> counters(d_counters.size());

  for(size_t i=0;i<d_counters.size();++i)
    counters[i]= (size_t)(d_counters[i].d_total >> skipBits);

  return counters;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



