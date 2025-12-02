#include "stddefx.h"
#include "calc_statement.h"
#include "calc_iscript.h"
#include "calc_quitforprogresscallback.h"
#include "calc_spatial.h"
#include "calc_position.h"

calc::Statement::Statement(const Element &pos) : Element(pos)
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

void calc::Statement::print(calc::InfoScript &) const
{
  POSTCOND(false);  // forgot an implementation
}
