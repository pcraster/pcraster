/*
  This code will initialize all
  global option with their default value
 */
STORAGE_CLASS(BOOL           ) app2dMatrix = FALSE;   /* 2dmatrix or 2dtable */
STORAGE_CLASS(BOOL           ) appDiagonal = TRUE;   /* diagonal or nondiagonal */
STORAGE_CLASS(BOOL           ) appPitOnBorder = TRUE; /* behaviour of pits on edge */
STORAGE_CLASS(BOOL           ) appUnitTrue = TRUE;    /* units true or cell */
STORAGE_CLASS(BOOL           ) appDouble = FALSE;    /* REAL4(default) or REAL8 */
STORAGE_CLASS(BOOL           ) appLarge  = FALSE;    /* UINT1(default) or INT4 */
STORAGE_CLASS(char*          ) appClone = NULL;      /* clone map */
STORAGE_CLASS(APP_COORD      ) appCoord = APP_C; /* centre, upperleft or lower right */
STORAGE_CLASS(APP_OUTPUT     ) appOutput = APP_OUT;/* progress, output or no output */
STORAGE_CLASS(APP_HEADER     ) appHeader = APP_DEFHEADER;/* default a tss or col header */
STORAGE_CLASS(APP_DIRECTION  ) appDirection = APP_DEGREES; /* radians or degrees */
STORAGE_CLASS(APP_LDDDEM_MOD ) appLddDemModifier = APP_LDDDEMFILL; /* fill, cut */
STORAGE_CLASS(APP_IO_STRATEGY) appIOstrategy = APP_IO_PCRASTER; /* do not read other formats */
STORAGE_CLASS(APP_DWR_TYPE   ) appDynamicWaveRoughness = APP_DWR_MANNING; /* see dynamicwave() */
STORAGE_CLASS(BOOL           ) appSaveWD = TRUE; /* should command save its working directory ? */
