#include "stddefx.h"

#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif
#ifndef INCLUDED_MATHX
#include "mathx.h"
#define INCLUDED_MATHX
#endif
#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif
#ifndef INCLUDED_APP
#include "app.h"
#define INCLUDED_APP
#endif
#ifndef INCLUDED_GEO_FILECREATETESTER
#include "geo_filecreatetester.h"
#define INCLUDED_GEO_FILECREATETESTER
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif


#define USAGE  \
 "USAGE: mapdiff [-pf differenceFile ] mapOrTss1 mapOrTss2\n" \
 " exits non-zero if differences with diagnostic on stderr\n" \
 "         -f write difference file\n" \
 "         -p write difference values as % (0.3=30%)\n"

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/


int main(int argc,      /* number of arguments */
  char *argv[])      /* list of arguments */
{
  int c;
  char *fileName=NULL;

  exitOnError=1;
  /* install application */

  appOutput = APP_NOOUT;
  if(InstallArgs(argc, argv, "pf*", "mapdiff", __DATE__))
     exit(1);

  bool percentage=false;
  while((c = GetOpt()) != 0)
  {
   switch(c) {
    case 'f':
      fileName = (char *)OptArg;
      break;
    case 'p':
      percentage=true;
      break;
   }
  }


  if ( (argv = ArgArguments(&argc)) == NULL)
    exit(1);

  if (AppArgCountCheck(argc,3,3,USAGE))
    exit(1);

  // mis use of file create tester
  geo::FileCreateTester t(argv[1],false);
  if (fileName)
    t.setDifferenceFile(fileName);

  t.setPercentageDifference(percentage);

  bool equal(false);
  try {
    equal =t.equalTo(argv[2]);
  } catch (const com::Exception& e) {
    std::cerr << e.messages();
    exit(1);
  }

  exit(!equal);
  return 0;
}
