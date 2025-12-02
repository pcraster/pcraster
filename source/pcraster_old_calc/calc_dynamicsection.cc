#include "stddefx.h"
#include "calc_dynamicsection.h"
#include "calc_infoscript.h"
#include "calc_iscript.h"
#include "com_cpucyclecounter.h"
#include "calc_operationtimer.h"
#include "calc_spatial.h"

void calc::DynamicSection::print(calc::InfoScript &i) const
{
  i.stream() << "<P><B>" << "DYNAMIC" << "</B><BR>\n";
  InnerStatementBlock::print(i);
}

void calc::DynamicSection::executeBlock()
{
  IScript &s(script());
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
