#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <time.h>

/* apps. called */
#include "app.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/
#define ZERO    ((time_t)0)
#define NOTIME  ((time_t)-1)

/*********************/
/* LOCAL DEFINITIONS */
/*********************/
static BOOL firstDynTimeCalled = TRUE;
static  time_t start  = ZERO;
static  time_t paused = ZERO;
static  char  timeBuf[16];

/******************/
/* IMPLEMENTATION */
/******************/

/* format time
 * StrTime formats a time, expressed in seconds.
 * Returns pointer to static buffer in format h:mm:ss.
 * Houres are printed in %2d-format.
 * Minutes and seconds are printed in %02d-format.
 */
static const char *StrTime(
 long t)  /* time in seconds */
{
 (void)sprintf(timeBuf,"%d:%02d:%02d",
  (int) (t / 3600),
  (int) (t % 3600)/60,
  (int) (t % 60));
 return timeBuf;
}

/* Start a stopwatch
 * StartTimer resets a stopwatch to zero that
 * can be read with ReadTimer
 *
 * WARNING:
 * Current implementation assumes time_t to be an integer type.
 */
static void StartTimer(void)
{
 (void)time(&start);
 POSTCOND(start != NOTIME);
}

/* Pause the stopwatch
 * Pause the stopwatch untill PauseOffTimer is called.
 * Repetive use has no effect, first call pauses till PauseOffTimer 
 */
static void PauseOnTimer(void)
{
 if (paused == ZERO)
  {
   (void)time(&paused);
   POSTCOND(paused != NOTIME);
  }
}

/* Continue the paused stopwatch 
 * PauseOffTimer continues the paused stopwatch.
 * Repetive use has no effect.
 */
static void PauseOffTimer(void)
{
 time_t now;

 if (paused != ZERO)
 {
  (void)time(&now);
  POSTCOND(now != NOTIME);
  start += (now - paused);
  paused = ZERO;
 }
}

/* Read stopwatch time
 * Returns
 * time in seconds since StartTimer was called minus the
 * intervals created by optional PauseOnTimer and PauseOffTimer
 */
static long ReadTimer(void)
{
  time_t now;

  if (paused != ZERO)
    return((long)(paused-start));

  (void)time(&now);
  POSTCOND(now != NOTIME);
  return((long)(now-start));
}

/* Prints time at line to show that program is running.
 */
void AppDynamicProgress(void)
{
 const char *time;
 if(appOutput != APP_PROGRESS)
  return;
 if(firstDynTimeCalled)
 {
  firstDynTimeCalled = FALSE;
  StartTimer();
  (void)fprintf(stderr,  "PROGRESS:\n");
 }
 time = StrTime(ReadTimer());
 (void)fprintf(stderr, "\r %s", time);
}

/* Prints new line to stderr.
 */
 void AppEndDynamicProgress(void)
 {
  if(appOutput == APP_PROGRESS)
   (void)fprintf(stderr, "\n");
 }
