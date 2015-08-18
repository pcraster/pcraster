#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "app.h"  /* GetOpt, ArgArguments, InstallArgs */
#include "misc.h"  /* ChkRealloc, StrcpTmpMalloc */
#include <string.h>  /* memmove, strlen, strchr, strcmp */
#include <ctype.h>  /* isspace */
#include "pcrshell.h" /* set directory */

#ifndef INCLUDED_APP_OPTIONS
#include "app_options.h"
#define INCLUDED_APP_OPTIONS
#endif

/* apps. called */

/*************/
/* EXTERNALS */
/*************/

char *licenceeName=NULL;
int   appUnitTest=FALSE;

/* list of dynamic library names
 * dynamicLibraryNames holds all dynamic libraries with the
 * --dynamiclibraries global option.
 * Names must be without an extension (dll,so).
 */
char *dynamicLibraryNames[64];

/* number of dynamic libraries in dynamicLibraryNames
 */
size_t nrDynamicLibraryNames=0;

/* pointer to flag argument
 * OptArg holds the flag argument.
 *
 * EXAMPLE
 * .so examples/exapp.tr
 */
const void *OptArg;

/* define if a negative number is a flag
 * If appNumbersAreArguments is set to TRUE (default FALSE) then negative numbers
 * are not recognized as flags.
 */
BOOL appNumbersAreArguments = FALSE;
/* define if all options should be most left
 * If appAllOptionsMostLeft is set to TRUE (default FALSE) then all arguments after
 * the first recognized arguments (non flag) are regarded as arguments, not flags.
 */
BOOL appAllOptionsMostLeft = FALSE;

/* initialized with defaults:
 */
#define STORAGE_CLASS(class) class
#include "global.inc"

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

# ifdef EVAL_VERSION
#  define VERSION "%s Evaluation-version: %s (%s)\n"
#  else
#   define VERSION "%s version: %s (%s)\n"
# endif

#define OPT_SYM_NONE 0
#define OPT_SYM_REAL 1   /* $ */
#define OPT_SYM_INT  2   /* # */
#define OPT_SYM_STR  3   /* * */
#define SET_OPT_SYM(i,value)  localFlagsOptions[i] = (localFlagsOptions[i]|((value)<<1))
#define GET_OPT_SYM(x)        ((localFlagsOptions[x])>>1)
#define SET_OPT_MULT(x)       localFlagsOptions[x] = ((localFlagsOptions[x])|1)
#define GET_OPT_MULT(x)       ((localFlagsOptions[x])&1)

#define SET_FLAG(flag, s, setFlag)  {if (StrEq(flag,s)){setFlag; return TRUE;}}

/* FLAG stuff (LIBRARY_INTERNAL)
 */
typedef struct FLAG {
  BOOL   set;
  double d;
  int    i;
  const char  *s;
  struct FLAG *next;
} FLAG;

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

static char *localFlags=NULL;
static int  nrLocFlags=0;
static char *localFlagsOptions=NULL;
static char *groupInput=NULL;
static char *groupResult=NULL;
/* TRUE if --clone flag is last one parsed
 * and its argument is not yet parsed
 */
static BOOL appCloneParsed = FALSE;
static FLAG *flagsSet;
static int nrFlagsSet=0;
static int nrMaxDiffFlagsSet;
static char **locArgv;
static int locArgc=0;

static int getOptNextFlag;
static FLAG *getOptFlagSet;

/******************/
/* IMPLEMENTATION */
/******************/

static BOOL NumbersAreArg(const char *arg)
{
   double d;
   return appNumbersAreArguments && CnvrtDouble(&d,arg);
}

static int InstallLocalOptions(
  const char *o) /* option string passed to InstallArgs */
{
        size_t i,n = strlen(o); /* big enough, too big */
        int groupInputLen = 0;

#     ifdef DEBUG
  /* Check on redefinition of format of options */
  for(i = 0; i < strlen(o); i++)
  {
    char a = o[i];
    if(isalnum(a))
     PRECOND(strchr(o+(i+1), a) == NULL);
  }
#     endif

        localFlags        = (char *)ChkMalloc((size_t)(n+1));
        groupInput        = (char *)ChkMalloc((size_t)(n+1));
        groupResult       = (char *)ChkMalloc((size_t)(n+1));
  if (localFlags == NULL || groupInput == NULL || groupResult == NULL)
    return 1;
        localFlagsOptions = (char *)ChkCalloc((size_t)(n+1), sizeof(char)); /* 0's are defaults */
  if (localFlagsOptions == NULL)
    return 1;
  for (i=0; i < n; i++ )
   switch(o[i]) {
    case '&' :
     PRECOND(nrLocFlags > 0);
     SET_OPT_MULT(nrLocFlags-1);
     break;
    case '$' :
     PRECOND(nrLocFlags > 0);
     PRECOND(GET_OPT_SYM(nrLocFlags-1)==OPT_SYM_NONE);
     SET_OPT_SYM(nrLocFlags-1,OPT_SYM_REAL);
     break;
    case '#' :
     PRECOND(nrLocFlags > 0);
     PRECOND(GET_OPT_SYM(nrLocFlags-1)==OPT_SYM_NONE);
     SET_OPT_SYM(nrLocFlags-1,OPT_SYM_INT);
     break;
    case '*' :
     PRECOND(nrLocFlags > 0);
     PRECOND(GET_OPT_SYM(nrLocFlags-1)==OPT_SYM_NONE);
     SET_OPT_SYM(nrLocFlags-1,OPT_SYM_STR);
     break;
    case '(' :
    case ')' :
     groupInput[groupInputLen++] = o[i];
     break;
    default : /* the flag itself or '\0' */
     PRECOND(isalnum(o[i]) || o[i] == '\0'); /* [a-z,A-Z,0-9] */
     localFlags[nrLocFlags++] = o[i];
     groupInput[groupInputLen++] = o[i];
   }
   localFlags[nrLocFlags] = '\0';
   groupInput[groupInputLen] = '\0';
   strcpy(groupResult, groupInput);
  return 0;
}

int SetClone(const char *cloneName)
{
  FREE_NULL(appClone);
  if (cloneName)
    if ((appClone = StrcpyChkMalloc(cloneName)) == NULL)
      return 1;
  return 0;
}

/* Checks whether given flag fits a global option.
 * Returns 1 if flag is global, - otherwise.
 */
int ParseGlobalFlag(
   const char *flag) /* InclDoubleDash */
{
  flag +=2; /* skip "--" */
  appCloneParsed = FALSE;

  SET_FLAG( flag, "clone",        appCloneParsed = TRUE)
  SET_FLAG( flag, "unittrue",     appUnitTrue = TRUE)
  SET_FLAG( flag, "unitcell",     appUnitTrue = FALSE)
  SET_FLAG( flag, "lddout",       appPitOnBorder = TRUE)
  SET_FLAG( flag, "lddin",        appPitOnBorder = FALSE)
  SET_FLAG( flag, "lddcut",       appLddDemModifier = APP_LDDDEMCUT)
  SET_FLAG( flag, "lddfill",      appLddDemModifier = APP_LDDDEMFILL)
  SET_FLAG( flag, "nondiagonal",  appDiagonal = FALSE)
  SET_FLAG( flag, "diagonal",     appDiagonal = TRUE)
  SET_FLAG( flag, "radians",      appDirection = APP_RADIANS)
  SET_FLAG( flag, "degrees",      appDirection = APP_DEGREES)
  SET_FLAG( flag, "coorcentre",   appCoord = APP_C)
  SET_FLAG( flag, "coorul",       appCoord = APP_UL)
  SET_FLAG( flag, "coorlr",       appCoord = APP_LR)
  SET_FLAG( flag, "nothing",      appOutput = APP_NOOUT)
  SET_FLAG( flag, "noprogress",   appOutput = APP_OUT)
  SET_FLAG( flag, "progress",     appOutput = APP_PROGRESS)
  SET_FLAG( flag, "noheader",     appHeader = APP_NOHEADER)
  SET_FLAG( flag, "defaultheader",appHeader = APP_DEFHEADER)
  SET_FLAG( flag, "dbheader",     appHeader = APP_DBHEADER)
  SET_FLAG( flag, "esrigrid",     appIOstrategy = APP_IO_ESRIGRID)
  SET_FLAG( flag, "pcraster",     appIOstrategy = APP_IO_PCRASTER)
  SET_FLAG( flag, "bandmap",      appIOstrategy = APP_IO_BANDMAP)
  SET_FLAG( flag, "single",       appDouble = FALSE)
  SET_FLAG( flag, "double",       appDouble = TRUE)
  SET_FLAG( flag, "small",        appLarge = FALSE)
  SET_FLAG( flag, "large",        appLarge = TRUE)
  SET_FLAG( flag, "matrixtable",  app2dMatrix = TRUE)
  SET_FLAG( flag, "columntable",  app2dMatrix = FALSE)
  SET_FLAG( flag, "chezy",        appDynamicWaveRoughness = APP_DWR_CHEZY)
  SET_FLAG( flag, "manning",      appDynamicWaveRoughness = APP_DWR_MANNING)
  SET_FLAG( flag, "savewd",       appSaveWD = TRUE)
  SET_FLAG( flag, "nosavewd",     appSaveWD = FALSE)

  if (strncmp(flag,"dynamiclibraries:",strlen("dynamiclibraries:")) == 0)
    return app_setDynamicLibraries(flag);

   return FALSE;      /*  flag does not fit */
}

SAVE_STRTOK createSaveStrtok(const char *s);
void deleteSaveStrtok(SAVE_STRTOK s);
const char* nextSaveStrtok(SAVE_STRTOK s);

/* scans environment variable for global options.
 * Returns 1 on case of failed memory allocation or crap in env. variable,
 * 0 otherwise.
 */
 static int ParseEnv(void)
 {
  SAVE_STRTOK ss;
  const char   *p;
  char *env = getenv("PCROPTIONS");
  if (env == NULL)
    return 0; /* ready */
  ss=createSaveStrtok(env);
  p = nextSaveStrtok(ss);
  while( p != NULL )
  {
    if (appCloneParsed) /* --clone just parsed, copy name */
    {
      if (SetClone(p))
        goto error;
      appCloneParsed = FALSE;
    }
    else if (!ParseGlobalFlag(p)) {
      Error("env. variable PCROPTIONS contains unknown"
           " global option: '%s'", p);
      goto error;
    }
    p = nextSaveStrtok(ss);
  }
  deleteSaveStrtok(ss);
  if (appCloneParsed)
     return RetError(1,"env. variable PCROPTIONS contains --clone"
          " option, but no clone map specified");
  return 0;
error:
  deleteSaveStrtok(ss);
  return 1;
} /* ParseEnv */

static int CheckSetFlag(
  FLAG *currFlag, /* write-only */
  int  i,         /* flag index */
  const char *a)  /* argument of index */
{
   PRECOND(a != NULL);
   PRECOND(a[0] != '\0');
   PRECOND(currFlag != NULL);

   switch(GET_OPT_SYM(i))  {
    case OPT_SYM_REAL:
           if (!CnvrtDouble(&(currFlag->d),a))
             return RetError(1,
              "argument of option '%c' is not a real number ('%s')",
              localFlags[i],a);
     break;
    case OPT_SYM_INT:
           if (!CnvrtInt(&(currFlag->i),a))
             return RetError(1,
              "argument of option '%c' is not an integer number ('%s')",
              localFlags[i],a);
           break;
          default:
     PRECOND(GET_OPT_SYM(i) == OPT_SYM_STR);
     currFlag->s = a;
  }
  return 0;
} /* CheckSetFlag */

static int ParseLocalFlags(
  int *locArgPtr,  /* read-write local arg ptr */
  int  argc)       /* nr of arguments */
{
  int i;
  const char *f = locArgv[*locArgPtr];
  FLAG *currFlag;

  PRECOND(f[0] == '-');

  for(i=1; f != NULL && f[i] != '\0'; i++)
  {
   char *flagPtr = strchr(localFlags, f[i]);
   int  flagIndex;
   if (flagPtr == NULL)
     return RetError(1,"Unknown option '%c'",f[i]);
   flagIndex = flagPtr-localFlags;

   /* find flag record
    */
   currFlag = flagsSet+flagIndex;
   if (GET_OPT_MULT(flagIndex) &&
       ( currFlag->set ) )
    { /* add another one
       */
     for ( ; currFlag->next != NULL; currFlag = currFlag->next)
               ;
     currFlag->next = flagsSet+(nrFlagsSet++);
     currFlag = currFlag->next;
    }
   currFlag->set = TRUE;
   if (GET_OPT_SYM(flagIndex) != OPT_SYM_NONE)
   { /* trailing argument neccessary
      */
      const char *flagArg = NULL;
      if (f[i+1] != '\0')
      { /* it's in this string
         */
         flagArg = f+(i+1);
         f = NULL; /* DONE */
      }
      else
      {
       if ( *locArgPtr < (argc-1) )
       { /* it's in next cmdline arg
          */
         locArgv[(*locArgPtr)++] = NULL;
         flagArg = locArgv[(*locArgPtr)];
       }
       else
        return RetError(1," option '%c' requires an argument",f[i]);
      }
      if (CheckSetFlag(currFlag, flagIndex, flagArg))
        return 1;
   }
      }
      return 0;
}

static int ParseArgv(
  int argc,    /* number of arguments */
  char *argv[])  /* read-only input line */
{
  int locArgPtr;
  locArgc = argc;
  locArgv = (char **)ChkMalloc(argc*sizeof(char *));

  nrFlagsSet = 1; /* let alloc succeed */
  for (locArgPtr=0; locArgPtr < argc; locArgPtr++)
  {
    locArgv[locArgPtr] = argv[locArgPtr];
    if (locArgPtr > 0 && argv[locArgPtr][0] == '-' && argv[locArgPtr][1] != '-')
     /* string of local options */
      nrFlagsSet += strlen(argv[locArgPtr])-1;
  }
  nrMaxDiffFlagsSet = nrLocFlags;
  /* Calloc: 0's are defaults */
  flagsSet = (FLAG *)ChkCalloc((size_t)(nrFlagsSet+nrMaxDiffFlagsSet) , sizeof(FLAG));
  if (flagsSet == NULL)
    return 1;
  nrFlagsSet = nrMaxDiffFlagsSet;
  for (locArgPtr=1; locArgPtr < argc; locArgPtr++)
  {
   BOOL arg = FALSE;
   if (!strncmp("--", locArgv[locArgPtr], (size_t)2))
   {
    appCloneParsed = FALSE;
    if (!ParseGlobalFlag(locArgv[locArgPtr]))
     return RetError(1,"command line contains unknown"
          " global option: '%s'", locArgv[locArgPtr]);
    if (appCloneParsed)
    {
      appCloneParsed = FALSE;
      locArgv[locArgPtr] = NULL; /* exclude from arguments */
      if (++locArgPtr < argc)
      {
       if (SetClone(locArgv[locArgPtr]))
         return 1;
      } else
          return RetError(1,"command line contains --clone"
                            " option, but no clone map specified");
    }
   } /* not a global option: */
  else if ( locArgv[locArgPtr][0] == '-'
            && !NumbersAreArg(locArgv[locArgPtr]))
             /* keep if it's a number and we want to keep
              * them
              */
        {
          if (ParseLocalFlags(&locArgPtr, argc))
            return 1;
        } else /* not an option */
           arg = TRUE;
   if (!arg)
        locArgv[locArgPtr] = NULL; /* exclude from arguments */
   else
    { /* first argument found
       */
       if (appAllOptionsMostLeft)
         break;
    }
       } /* eofor */

       /* resize options to smaller size
       * should work always
        */
       if (nrFlagsSet != 0)
       {
   ChkReallocFree((void **)&flagsSet, nrFlagsSet * sizeof(FLAG));
   POSTCOND(flagsSet != NULL);
       }
    return 0;
}

static BOOL GroupCheck(void)
{
  enum state { SINGLE, GROUP } state;
  int  groupMemberUsed = -1; /* FOR DEBUG */
  int i,n = strlen(groupResult);
  PRECOND(groupResult);
  state = SINGLE;
  for(i=0; i < n; i++)
  {
    if(state==SINGLE)
     switch(groupResult[i]) {
      case '(' : state = GROUP;
                       groupMemberUsed = 0;
                            /* index, initial none used
                             * 0 is safe since group starts
                             * with '(', so a valid value
                             * will be larger than 0
                             */
                       break;
      case ')' : PRECOND(FALSE); break;
     }
    else
    {
     PRECOND(groupMemberUsed >= 0);
     switch(groupResult[i]) {
      case '-' : if (!groupMemberUsed)
                  groupMemberUsed = i;
                 else
                  return
                    RetError(0,"Options '-%c' and '-%c' are mutually exclusive "
                               ", specify only one of them",
                               groupInput[groupMemberUsed],
                               groupInput[i]);
                 break;
      case ')' : state = SINGLE;
                 break;
      default  : POSTCOND(groupResult[i] != '(');
     }
    }
  }
  return 1;
}

int AppParseShellLine(
  const char *firstLine)
{
  SAVE_STRTOK ss = createSaveStrtok(firstLine);
  const char *p;
  PRECOND(!appCloneParsed);
  p= nextSaveStrtok(ss);
  while( p != NULL )
  {
    if (appCloneParsed) /* --clone just parsed, copy name */
    {
      if (SetClone(p))
        goto error;
      appCloneParsed = FALSE;
    }
    else
      if (!strncmp("--", p, (size_t)2))
        if (!ParseGlobalFlag(p)) {
           Error("the #! line contains unknown"
                 " global option: '%s'", p);
           goto error;
         }
    p= nextSaveStrtok(ss);
  }
  deleteSaveStrtok(ss);
  if (appCloneParsed)
     return RetError(1,"the #! line of contains --clone"
          " option, but no clone map specified");
  return 0;
error:
  deleteSaveStrtok(ss);
  return 1;
}


/* Gives the arguments and check the result of flag processing.
 * ArgArgument gives the arguments that are not identified
 * as flags/options in InstallArgs(). It also checks if more than
 * one flag of a group is given (which is an error).
 *
 * Returns pointer to parameters or NULL if flag group check fails
 * (error message is printed).
 *
 * EXAMPLE
 * .so examples/exapp.tr
 */
char **ArgArguments(int *nrArgs)    /* write-only number of arguments */
{
  int src, dest=0;
  /* shift ptrs to begin */
  for(src=0; src < locArgc; src++)
    if (locArgv[src] != NULL)
      locArgv[dest++] = locArgv[src];
  *nrArgs = dest;
  return GroupCheck() ? locArgv : NULL;
}

/* give options one by one, setting OptArg each time.
 * Checks whether arguments are expected and whether or not they have
 * a valid value. In case of overruling, the last given argument is
 * considered. (Left to right and command line rules over environment
 * setting).
 * Sets the extern OptArg pointer to some static buffer denoting a pointer
 * to double, int or char.
 * Returns option character or 0 if all local options are processed.
 *
 * EXAMPLE
 * .so examples/exapp.tr
 */
 int GetOpt(void)
 {
  int nextFlag;
  char *grPtr;
  if (getOptNextFlag >= nrMaxDiffFlagsSet || flagsSet == NULL)
    return 0;

  if (getOptFlagSet == NULL)
  { /* find new one */
   for( ; getOptNextFlag < nrMaxDiffFlagsSet; getOptNextFlag++)
   {
    getOptFlagSet = flagsSet+getOptNextFlag;
    if (getOptFlagSet->set)
      break;
   }
   if (getOptNextFlag == nrMaxDiffFlagsSet)
     return 0;
  }

        switch(GET_OPT_SYM(getOptNextFlag))  {
           case OPT_SYM_REAL: OptArg = &(getOptFlagSet->d); break;
           case OPT_SYM_INT:  OptArg = &(getOptFlagSet->i); break;
     case OPT_SYM_STR:  OptArg = getOptFlagSet->s; break;
     default:           OptArg = NULL;
  }
  getOptFlagSet = getOptFlagSet->next;
  if (getOptFlagSet == NULL)
   nextFlag= localFlags[getOptNextFlag++];
  else
   nextFlag= localFlags[getOptNextFlag];
        grPtr = strchr(groupResult, nextFlag);
        if (grPtr != NULL) /* NULL possible if flag has multiple option */
    *grPtr = '-'; /* mark used */
  return nextFlag;
}

/* Parses the command line with options, arguments and file names.
 * Scans and set the
 * global options. Checks the environment variable on containing no
 * illegal global options.
 *
 * The  parameter options is a string with the characters that are local
 * options. A character [a-zA-Z0-9] can be followed by a parameter-symbol to identify that
 * the option has a parameter
 * (like -DDEBUG, D is the option, DEBUG is the parameter)
 * and the type of the parameter
 *
 * parameter-symbols are [plus type(cast) of OptArg]:
 *
 * $  [ (const double *) ]
 *
 * #  [ (const int *) ]
 *
 * *  [ (const char *) ]
 *
 * The character (and parameter-symbols) can be followed a &-symbol
 * in the options argument to identify that the option can be passed more then
 * once to the program. If options are mutual exclusive they can be specified in
 * paranthesis for an extra check, e.g. (ef): the user is wrong if he gives both
 * the e and f option.
 * If appNumbersAreArguments is set to TRUE (default FALSE) then negative numbers
 * are not recognized as flags.
 * If appAllOptionsMostLeft is set to TRUE (default FALSE) then all arguments after
 * the first recognized arguments (non flag) are regarded as arguments, not flags.
 * Returns 1 in case of failed memory allocation, 0 otherwise.
 *
 * EXAMPLE
 * .so examples/exapp.tr
 */
int InstallArgs(
  int argc,    /* number of arguments */
  char **argv,    /* read-only input line, actually const */
  const char *options,  /* option list */
  const char *progName,  /* name of program */
  const char *version)  /* version of program */
{

  AppSetGlobalArgsDefaults();
  if (InstallLocalOptions(options))
    goto error;


  /* parse global options set in environment variable */
  if (ParseEnv())
     goto error;
  if  (ParseArgv(argc,argv))
     goto error;

  /* init GetOpt processing */
  getOptNextFlag = 0;
  getOptFlagSet  = NULL;

  /*
   * AppLogStart(argc,argv);
   * CW_MUTATE SEND: DATE,DIRECTORY,ARGS to logger 
   */
#ifdef WIN32
   if (appSaveWD)
      (void)setPCRShellDirectoryToCurrent();
#endif

  if(appOutput != APP_NOOUT && !appUnitTest)
  {
#      ifdef DEBUG_DEVELOP
  (void)fprintf(stderr,"PCRTEAM VERSION, INTERNAL USE ONLY! (%s)\n",
   PLATFORM_TXT);
#      endif
   (void)fprintf(stderr, VERSION, progName, version,PLATFORM_TXT);
   if (licenceeName != NULL)
   (void)fprintf(stderr, "licenced to %s\n\n",licenceeName);
  }
  return 0;
error:
  AppEnd();
  return 1;
}

void AppSetGlobalArgsDefaults(void)
{
  FREE_NULL(appClone);

  /* re-initialize settings of global variables.
   */
#  undef STORAGE_CLASS
#  define STORAGE_CLASS(class)
#  include "global.inc"
}

/* check if the app has enough arguments, print usage if none given
 * AppArgCountCheck generates error messages if nessary
 * and prints the usage string if argc is 1
 * return 0 if the nr. of arguments are ok, 1 if not
 */
int AppArgCountCheck(
  int argc,    /* the number of arguments >= 1 */
  int minArgc, /* the minimum number of arguments */
  int maxArgc, /* the maximum number of arguments, -1 if
                * unlimited  number of arguments is
                * possible
                */
  const char *usage) /* the usage message */
{
  if (argc == 1) {
    (void)fprintf(stderr, "%s", usage);
    return 1;
  }
  if (argc < minArgc)
   return RetError(1,"Not enough arguments");
  if (maxArgc != -1 && argc > maxArgc)
   return RetError(1,"Too many arguments");
  return 0;
}

/* Clean up internal data used by InstallArgs, GetOpt, etc.
 *
 * EXAMPLE
 * .so examples/exapp.tr
 */
void AppEnd(void)
{
  size_t i;

  FREE_NULL(appClone);
  EndGetOpt();

  for (i=0; i < nrDynamicLibraryNames; i++)
      free(dynamicLibraryNames[i]);
  nrDynamicLibraryNames=0;
}

void EndGetOpt(void)
{
  nrLocFlags=0;
  nrFlagsSet=0;
  locArgc=0;
  FREE_NULL(localFlags);
  FREE_NULL(localFlagsOptions);
  FREE_NULL(flagsSet);
  FREE_NULL(locArgv);
  FREE_NULL(groupInput);
  FREE_NULL(groupResult);
}


#ifdef NEVER
int main(
 int argc,  /* number of arguments */
 char **argv)  /* list of arguments */
{
 if(InstallArgs(argc, argv, "t&h$x#m*S", "name", "1.01"))
  exit(1);

 argv = ArgArguments(&argc);

 AppEnd();
 exit(0);
 return 0;
}
#endif
