#include "stddefx.h"

#ifndef INCLUDED_CALC_STATEMENT
# include "calc_statement.h"
#define INCLUDED_CALC_STATEMENT
#endif
#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif
#ifndef INCLUDED_CALC_QUITFORPROGRESSCALLBACK
#include "calc_quitforprogresscallback.h"
#define INCLUDED_CALC_QUITFORPROGRESSCALLBACK
#endif

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_CALC_POSITION
#include "calc_position.h"
#define INCLUDED_CALC_POSITION
#endif

calc::Statement::Statement(
  const Element& pos):
   Element(pos)
{
}

calc::Statement::~Statement()
{
}

//! actual execution by calling Statement::run()
/*!
 * \todo
 *   harmonize naming of execute, run and start methods
 * \todo
 *   push ProgressCallBack into expressions, now on statement level
 */
void calc::Statement::start()
{
  script().updateProgress(calc::StatementPulse);
//  std::cout << "running " << position()->text() << "\n";
//                  Spatial::maxBPC() << " " << Spatial::currentBPC() << "\n";
//  POSTCOND(Spatial::currentBPC() < 24000);

  run();
}

void calc::Statement::print(calc::InfoScript&) const
{
  POSTCOND(FALSE); // forgot an implementation
}
