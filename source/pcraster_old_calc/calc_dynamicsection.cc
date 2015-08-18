#include "stddefx.h"

#ifndef INCLUDED_CALC_DYNAMICSECTION
#include "calc_dynamicsection.h"
#define INCLUDED_CALC_DYNAMICSECTION
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_COM_CPUCYCLECOUNTER
#include "com_cpucyclecounter.h"
#define INCLUDED_COM_CPUCYCLECOUNTER
#endif

#ifndef INCLUDED_CALC_OPERATIONTIMER
#include "calc_operationtimer.h"
#define INCLUDED_CALC_OPERATIONTIMER
#endif

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

void calc::DynamicSection::print(calc::InfoScript& i)const
{
  i.stream() << "<P><B>" << "DYNAMIC" << "</B><BR>\n";
  InnerStatementBlock::print(i);
}

void calc::DynamicSection::executeBlock()
{
  IScript& s(script());
  do {
    s.nextTimeStep();
/*
 *   if (0) std::cout       <<
 *    "time (todo) " << s.currentTimeStep()   << "\t" <<
 *    "current bpc " << Spatial::currentBPC() << "\t" <<
 *    "maximal bpc " << Spatial::maxBPC()     << "\n" ;
 */
    /*
     * if (s.currentTimeStep() == 3) {
     *   startCpuCycleCounter(0);
     *   calc::OperationTimer::setTimerOn(true);
     * }
     * if (s.currentTimeStep() == 703) {
     *   calc::OperationTimer::setTimerOn(false);
     *   stopCpuCycleCounter(0);
     * }
     */

    s.updateProgress(LoopPulse);
    executeStatements();
  } while (s.currentTimeStep() != s.nrTimeSteps());
/*
 * size_t skipBits=18;
 * writeCpuCycleCounterStats(skipBits);
 * calc::OperationTimer::print(skipBits);
 */
}

bool calc::DynamicSection::inDynamic() const
{
  return true;
}
