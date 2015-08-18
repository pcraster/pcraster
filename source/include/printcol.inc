static void MakeFmts(
              char *valFmt_s,
              char *mvStr_s,
              CSF_VS vs_s,     /* set to scalar for X,Y */
	const char *userFmt_s,  /* empty if not specified */
	const char *mv_s,
	const MAP  *map_s)  /* NULL if not used, for X,Y */
{
	size_t strLen, strLenMv;
	char mvFmt[128],fmt[128];

	(void)strcpy(fmt, userFmt_s);
	strLen   = atoi(fmt);
	strLenMv = MAX(strlen(mv_s), strLen);
	POSTCOND(strLenMv > 0);

	if ( EmptyString(fmt)) /* fmt not given by user */
	{
		switch(vs_s){
		 case VS_LDD     : 
		 case VS_BOOLEAN :
		                 (void)sprintf(fmt,"%u.0f",(UINT_T)strLenMv);
		                 break;
		 case VS_CLASSIFIED:
		 case VS_ORDINAL   :
		 case VS_NOMINAL   :
		               { char minS[32], maxS[32];
		                 REAL8 min,max;
		                 RgetMinVal(map_s,&min);
		                 RgetMaxVal(map_s,&max);
		                 (void)sprintf(minS,"%g",min);
		                 (void)sprintf(maxS,"%g",max);
		                 strLen   = MAX(strlen(minS),strlen(maxS));
				 strLenMv = MAX(strLenMv, strLen);
		                 (void)sprintf(fmt,"%u.0f",(UINT_T)strLenMv);
		               } break;
		 default      :  (void)strcpy(fmt,"11.6g");
		                 strLen   = strLenMv = 11;
                }
        }

	/* fill buf with '%' followed by print format for values */
	valFmt_s[0] = '%';
	(void)strcpy(valFmt_s+1, fmt);
        (void)sprintf(mvFmt,"%%%us",(UINT_T)strLenMv);

	(void)sprintf(mvStr_s,mvFmt,mv_s);
}

/* Determines the value of a cell and puts it in the output file.
 * Returns 1 in case of write error, 2 if mv occured, 0 otherwise.
 */
static int PutCell(
	FILE *outputFile,	/* write-only output file */
	REAL8 *val,		/* read-write, value to put
	                         * only re-assigned if vs is DIRECTIONAL */
	CSF_VS vs,
	BOOL  number_s,
	REAL8 mvVal_s,
	const char *valFmt_s,
	const char *mvStr_s,
	const char *sep,        /* NULL if none */
	BOOL  firstOutputCol)   /* value for first output column? */
{
        char buf[256];

 	/* Determine and print value to output file */
 	if(!IS_MV_REAL8(val))
	{
		if(vs == VS_DIRECTION)
			*val = AppOutputDirection(*val);

 		if(number_s && mvVal_s == *val)
 			return 2;

		/* put value in output column file */
		(void)sprintf(buf, valFmt_s, *val);
 	}
 	else
		(void)strcpy(buf, mvStr_s);
	if (sep != NULL)
	{
		LeftRightTrim(buf);
		if (!firstOutputCol)
			(void)fprintf(outputFile, "%s", sep);
	}
	else 
		(void)fprintf(outputFile," ");

	return fprintf(outputFile, "%s", buf) < 0;
}

static void PrintError(
	int res,
	int r,
	int c,
	const REAL8 *val)
{
	if (res == 2)
        {
          PRECOND(! IS_MV_REAL8(val));
          ErrorNested("cell (row=%d,col=%d) has a value equal "
                      "to the specified missing value (%g)", r,c,*val);
	}
}
