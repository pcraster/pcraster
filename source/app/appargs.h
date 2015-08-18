#ifndef INCLUDED_APPARGS
#define INCLUDED_APPARGS

/* stuf to install option from command
 * line and retrieve those values
 */

#ifdef __cplusplus
 extern "C" {
#endif


typedef enum APP_COORD{
  APP_C,      /* center */
  APP_UL,      /* upper left */
  APP_LR      /* lower right */
}APP_COORD;
typedef enum APP_DIRECTION{
    APP_RADIANS,    /* direction in radians */
    APP_DEGREES    /* direction in degrees */
}APP_DIRECTION;
typedef enum APP_OUTPUT{
    APP_NOOUT,    /* no output */
    APP_OUT,    /* output */
    APP_PROGRESS    /* output during progress */
}APP_OUTPUT;
typedef enum APP_HEADER {
    APP_DEFHEADER,    /* default header */
    APP_NOHEADER,    /* no headers at all */
    APP_DBHEADER    /* database header        */
}APP_HEADER;
typedef enum APP_LDDDEM_MOD{
    APP_LDDDEMFILL,    /* dem is filled, when ldd modified */
    APP_LDDDEMCUT    /* dem is cut, when ldd modfied */
}APP_LDDDEM_MOD;

typedef enum APP_IO_STRATEGY {
  APP_IO_ESRIGRID,  /* esri grids are read and written */
  APP_IO_PCRASTER,  /* only our own format */
  APP_IO_BANDMAP    /* WL/Mapper BandMap format */
}APP_IO_STRATEGY;

typedef enum APP_DWR_TYPE { /* see dynwave.cc for details */
  APP_DWR_CHEZY,      /* Chezy */
  APP_DWR_MANNING     /* Manning */
}APP_DWR_TYPE;


/* global variables
 * defined in libapp.a (appinst.c)
 */
/* for argument parsing */
extern const void *OptArg;
extern BOOL appNumbersAreArguments;
extern BOOL appAllOptionsMostLeft;

/* libraries loaded */
extern char *dynamicLibraryNames[64];
extern size_t nrDynamicLibraryNames;

/* global option settings */
extern BOOL appDiagonal;  /* diagonal or nondiagonal */
extern BOOL app2dMatrix;  /* 2dmatrix or 2dtable */
extern BOOL appPitOnBorder;  /* behaviour of pits on edge */
extern BOOL appUnitTrue;  /* units true or pixel */
extern BOOL appDouble;    /* REAL4 or REAL8 */
extern BOOL appLarge;    /* UINT1 or INT4 */
extern char* appClone;    /* clone map */
extern APP_LDDDEM_MOD appLddDemModifier;/* fill or cut of dem */
extern APP_COORD appCoord;     /* centre, upperleft or lower right */
extern APP_DIRECTION appDirection;   /* radians or degrees */
extern APP_OUTPUT appOutput;     /* progress, output or no output */
extern APP_HEADER appHeader;
extern APP_IO_STRATEGY appIOstrategy;
extern APP_DWR_TYPE    appDynamicWaveRoughness; /* see dynamicwave() */
extern BOOL appSaveWD;    /* if command should save its working directory */

/*******************/
/*   PROTOTYPES    */
/*******************/

/* appinst.c */

extern int AppParseShellLine(const char *firstLine);
extern int AppArgCountCheck(int argc, int minArgc, int maxArgc,

                              const char *usage);
extern char **ArgArguments(int *nrArgs);

extern int GetOpt(void);

void AppSetGlobalArgsDefaults(void);

extern int ParseGlobalFlag(const char *flagInclDoubleDash);
extern int SetClone(const char *cloneName);

extern int InstallArgs(
    int argc,    /* number of arguments */
    char **argv,  /* read-only input line */
    const char *options,  /* local options */
    const char *progName,  /* name of program */
    const char *version);  /* version of program */

extern void AppEnd(void);
extern void EndGetOpt(void);

extern void AppProgress(
    const char *fmt,
    ...);

extern void AppVerbose(
    const char *fmt,
    ...);

extern void AppRowProgress(int r);

extern void AppEndRowProgress(void);

extern void AppUsageError(
    const char *fmt,
    ...);

extern void AppMemoryError(void);


extern double AppInputDirection(double radOrDeg);

extern double AppOutputDirection(double rad);

extern void AppFileOpenError(const char *filename);

#ifdef __cplusplus
 }
#endif

#endif /* INCLUDED_APPARGS */
