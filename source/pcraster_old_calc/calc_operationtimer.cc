#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OPERATIONTIMER
#include "calc_operationtimer.h"
#define INCLUDED_CALC_OPERATIONTIMER
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_CPUCYCLECOUNTER
#include "com_cpucyclecounter.h"
#define INCLUDED_COM_CPUCYCLECOUNTER
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the OperationTimer class.
*/



//------------------------------------------------------------------------------

#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_STACK
#include <stack>
#define INCLUDED_STACK
#endif
namespace  calc {

typedef std::map<std::string, Uint64> TotalTimes;
typedef std::map<std::string, size_t> NrCounts;
static TotalTimes totalTimes;
static NrCounts   nrCounts;

static bool      timerOn=false;

class TimerStack : public std::stack<std::string> {
     Uint64 d_topStart;
   void addTop() {
      Uint64 timeOnTop = cpuCycleCounter()-d_topStart;
      PRECOND(totalTimes.count(top()));
      totalTimes[top()] += timeOnTop;
   }
  public:
   void start(const std::string& operationId) {
      if (!empty())
          addTop();
      push(operationId);
      d_topStart = cpuCycleCounter();
    }
   void stop() {
      PRECOND(!empty());
      addTop();
      pop();
      d_topStart = cpuCycleCounter();
    }
};

static TimerStack timerStack;

};

//------------------------------------------------------------------------------
// DEFINITION OF STATIC OPERATIONTIMER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF OPERATIONTIMER MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::OperationTimer::OperationTimer(const std::string& operationId, bool count)
{
  if ( !timerOn)
    return;
  // init a new one
  if (totalTimes.count(operationId) == 0) {
    totalTimes[operationId] = 0;
    nrCounts[operationId] = 0;
  }
  timerStack.start(operationId);
  if (count)
    nrCounts[operationId]++;
}

//! dtor
calc::OperationTimer::~OperationTimer()
{
  if (!timerOn)
    return;
  timerStack.stop();
}

void calc::OperationTimer::print(size_t skipBits)
{
  Uint64 total=0;
  // strip some insignificant digits
  for(TotalTimes::iterator p=totalTimes.begin();
                           p!=totalTimes.end(); ++p) {
    p->second >>= skipBits; // if 10 then strip 1024
  }

  for(TotalTimes::iterator p=totalTimes.begin();
                           p!=totalTimes.end(); ++p) {
    std::cout << p->second << "\t" << nrCounts[p->first]
              << "\t" << p->first << "\n";
    total += p->second;
  }
  if (total > (1<<31) )
   std::cout << "Warning Total larger then 2^31 (" << (1<<31) << ")\n";
  std::cout << "Total" << "\t" << total << "\n";
  std::cout << "2^31 = (" << (((size_t)1)<<31) << ")\n";
}

void calc::OperationTimer::setTimerOn(bool on)
{
  timerOn=on;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



