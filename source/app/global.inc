/*
  This code will initialize all
  global option with their default value
 */
STORAGE_CLASS(bool           ) app2dMatrix = false;   /* 2dmatrix or 2dtable */
STORAGE_CLASS(bool           ) appDiagonal = true;   /* diagonal or nondiagonal */
STORAGE_CLASS(bool           ) appPitOnBorder = true; /* behaviour of pits on edge */
STORAGE_CLASS(bool           ) appUnitTrue = true;    /* units true or cell */
STORAGE_CLASS(bool           ) appDouble = false;    /* REAL4(default) or REAL8 */
STORAGE_CLASS(bool           ) appLarge  = false;    /* UINT1(default) or INT4 */
STORAGE_CLASS(char*          ) appClone = NULL;      /* clone map */
STORAGE_CLASS(APP_COORD      ) appCoord = APP_C; /* centre, upperleft or lower right */
STORAGE_CLASS(APP_OUTPUT     ) appOutput = APP_OUT;/* progress, output or no output */
STORAGE_CLASS(APP_HEADER     ) appHeader = APP_DEFHEADER;/* default a tss or col header */
STORAGE_CLASS(APP_DIRECTION  ) appDirection = APP_DEGREES; /* radians or degrees */
STORAGE_CLASS(APP_LDDDEM_MOD ) appLddDemModifier = APP_LDDDEMFILL; /* fill, cut */
STORAGE_CLASS(APP_IO_STRATEGY) appIOstrategy = APP_IO_PCRASTER; /* do not read other formats */
STORAGE_CLASS(APP_DWR_TYPE   ) appDynamicWaveRoughness = APP_DWR_MANNING; /* see dynamicwave() */
STORAGE_CLASS(bool           ) appSaveWD = true; /* should command save its working directory ? */
