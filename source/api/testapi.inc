/* DO NOT EDIT: CREATED FROM testapi.tem */
#line 1 "testapi.tem"
/* vim: syntax=c
 */

/* UINT1 */

MAP_UINT1 *ReadUINT1(
  const char *nameOrValue, size_t nrRows, size_t nrCols)
{
 UINT1 val;
 if (CnvrtUINT1(&val, nameOrValue))
  return InitMapUINT1(nrRows,nrCols,&val,FALSE,
                                                      CR_UINT1);
else
  {
   REAL8 dummy;
   if (CnvrtREAL8(&dummy, nameOrValue))
   { /* it's a number but not a valid one for UINT1
      */
     Error(" (in ReadUINT1) '%s' is not a valid UINT1",nameOrValue);
     return NULL;
   }
     return ReadUINT1Map(nameOrValue);
  }
}

MAP_UINT1 *ReadUINT1Map(const char *name)
{
  MAP *m;
        MAP_UINT1 *map;
        UINT1 max;
        UINT2 inCr;   /* in memory cellRepr */
        UINT2 fileCr; /* in file cellRepr */
        void *buf;

  PRECOND(name != NULL);
  m = Mopen(name, M_READ);
  if (m == NULL)
    MperrorExit(name,1);

  fileCr = RgetCellRepr(m);

  switch(fileCr) {
    case CR_INT1:
    case CR_INT2:
    case CR_UINT2:
    case CR_INT4:  inCr = CR_INT4; break;
    case CR_UINT1: inCr = CR_UINT1; break;
    case CR_REAL4:
    case CR_REAL8: inCr = CR_REAL4; break;
    case CR_UINT4:
    default      :
               Error(" '%s' has CR_UINT4 or illegal celRepr(%d)",
                    name, (int)fileCr);
               goto errExit;
        }
        if ( (CELLSIZE(inCr) > CELLSIZE(CR_UINT1))
              /* grabs all except: */ ||
             (inCr == CR_REAL4 && CR_UINT1 == CR_INT4)
           ) {
           Error(" (in ReadUINT1Map) '%s' is '%s' (too big for UINT1)",
                    name, RstrCellRepr(RgetCellRepr(m)));
    goto errExit; }

        if (RuseAs(m, inCr))
        {
          Error("RuseAs: Can't open '%s' as cellRepr '%d'\n",
                  name, (int)inCr);
          goto errExit;
        }
        Register(m);
        {
           size_t nrRows=RgetNrRows(m);
           size_t nrCols=RgetNrCols(m);
           /* first register then create */
           buf = ReadMapContents(m,  inCr,nrRows,nrCols);
          if (buf == NULL)
            goto errExit;
          map = InitMapUINT1(nrRows,nrCols,
                  buf,TRUE, inCr);
          if (map == NULL)
          {
            Free(buf);
            goto errExit;
          }
          RgetMaxVal(m, &max);
          map->maxVal = (int)max;
          map->valueScale = RgetValueScale(m);
        }
  return map;
errExit:
        Mclose(m);
        return NULL;
}

void WriteUINT1Map(const char *name,
                   MAP_UINT1 *map,
                   CSF_VS       valueScale)
{

  MAP *m;
  CSF_CR fileInCr = DetermineCellRepr(valueScale);
  /* fileInCr is different from m-?inCr */

  PRECOND(name != NULL);
  PRECOND(map != NULL);

  if ( fileInCr == CR_UINT2)
  {
    Error("(writing %s) valueScale is not type 2\n"
          " (probably inherited from an input map)", name);
    exit(1);
  }

  if (CELLSIZE(fileInCr) > CELLSIZE(CR_UINT1))
  {
   Error("(writing %s) valueScale does not support cellrepr UINT1\n",
    name);
   exit(1);
  }

  m = NewMap(name, fileInCr, valueScale);
  if (m == NULL)
    MperrorExit(name,1);
  if (RuseAs(m, map->inCellRepr))
    MperrorExit(name,1);
  RputSomeCells(m, 0, (UINT4)(map->nrRows*map->nrCols), map->spatialValue[0]);
  Mclose(m);
}

void DeleteMAP_UINT1(
  MAP_UINT1 *m)
{
  if (m->spatial)
    Free(m->spatialValue[0]);
  DeleteInternalMAP_UINT1(m);
}

MAP_UINT1 *CreateSpatialUINT1(CSF_CR cr,size_t nrRows, size_t nrCols)
{
  MAP_UINT1 *map;
  void *buf;
  PRECOND(TestApiInitTest("CreateSpatialUINT1"));
  if (cr != CR_UINT1 && cr != CR_INT4 && cr != CR_REAL8 && cr != CR_REAL4)
        {
    Error("CreateSpatialUINT1 cr must be UINT1,INT4,REAL4 or REAL8");
    return NULL;
  }
  if (cr == CR_REAL8)
      cr = CR_REAL4;
  buf = ChkMalloc( (size_t)(nrRows*nrCols*CELLSIZE(cr)));
  if (buf == NULL)
      return NULL;
  map = InitMapUINT1(nrRows,nrCols, buf,TRUE, cr);
  if (map == NULL) {
      Free(buf);
      return NULL;
  }
  return map;
}
#line 1 "testapi.tem"
/* vim: syntax=c
 */

/* INT4 */

MAP_INT4 *ReadINT4(
  const char *nameOrValue, size_t nrRows, size_t nrCols)
{
 INT4 val;
 if (CnvrtINT4(&val, nameOrValue))
  return InitMapINT4(nrRows,nrCols,&val,FALSE,
                                                      CR_INT4);
else
  {
   REAL8 dummy;
   if (CnvrtREAL8(&dummy, nameOrValue))
   { /* it's a number but not a valid one for INT4
      */
     Error(" (in ReadINT4) '%s' is not a valid INT4",nameOrValue);
     return NULL;
   }
     return ReadINT4Map(nameOrValue);
  }
}

MAP_INT4 *ReadINT4Map(const char *name)
{
  MAP *m;
        MAP_INT4 *map;
        INT4 max;
        UINT2 inCr;   /* in memory cellRepr */
        UINT2 fileCr; /* in file cellRepr */
        void *buf;

  PRECOND(name != NULL);
  m = Mopen(name, M_READ);
  if (m == NULL)
    MperrorExit(name,1);

  fileCr = RgetCellRepr(m);

  switch(fileCr) {
    case CR_INT1:
    case CR_INT2:
    case CR_UINT2:
    case CR_INT4:  inCr = CR_INT4; break;
    case CR_UINT1: inCr = CR_UINT1; break;
    case CR_REAL4:
    case CR_REAL8: inCr = CR_REAL4; break;
    case CR_UINT4:
    default      :
               Error(" '%s' has CR_UINT4 or illegal celRepr(%d)",
                    name, (int)fileCr);
               goto errExit;
        }
        if ( (CELLSIZE(inCr) > CELLSIZE(CR_INT4))
              /* grabs all except: */ ||
             (inCr == CR_REAL4 && CR_INT4 == CR_INT4)
           ) {
           Error(" (in ReadINT4Map) '%s' is '%s' (too big for INT4)",
                    name, RstrCellRepr(RgetCellRepr(m)));
    goto errExit; }

        if (RuseAs(m, inCr))
        {
          Error("RuseAs: Can't open '%s' as cellRepr '%d'\n",
                  name, (int)inCr);
          goto errExit;
        }
        Register(m);
        {
           size_t nrRows=RgetNrRows(m);
           size_t nrCols=RgetNrCols(m);
           /* first register then create */
           buf = ReadMapContents(m,  inCr,nrRows,nrCols);
          if (buf == NULL)
            goto errExit;
          map = InitMapINT4(nrRows,nrCols,
                  buf,TRUE, inCr);
          if (map == NULL)
          {
            Free(buf);
            goto errExit;
          }
          RgetMaxVal(m, &max);
          map->maxVal = (int)max;
          map->valueScale = RgetValueScale(m);
        }
  return map;
errExit:
        Mclose(m);
        return NULL;
}

void WriteINT4Map(const char *name,
                   MAP_INT4 *map,
                   CSF_VS       valueScale)
{

  MAP *m;
  CSF_CR fileInCr = DetermineCellRepr(valueScale);
  /* fileInCr is different from m-?inCr */

  PRECOND(name != NULL);
  PRECOND(map != NULL);

  if ( fileInCr == CR_UINT2)
  {
    Error("(writing %s) valueScale is not type 2\n"
          " (probably inherited from an input map)", name);
    exit(1);
  }

  if (CELLSIZE(fileInCr) > CELLSIZE(CR_INT4))
  {
   Error("(writing %s) valueScale does not support cellrepr INT4\n",
    name);
   exit(1);
  }

  m = NewMap(name, fileInCr, valueScale);
  if (m == NULL)
    MperrorExit(name,1);
  if (RuseAs(m, map->inCellRepr))
    MperrorExit(name,1);
  RputSomeCells(m, 0, (UINT4)(map->nrRows*map->nrCols), map->spatialValue[0]);
  Mclose(m);
}

void DeleteMAP_INT4(
  MAP_INT4 *m)
{
  if (m->spatial)
    Free(m->spatialValue[0]);
  DeleteInternalMAP_INT4(m);
}

MAP_INT4 *CreateSpatialINT4(CSF_CR cr,size_t nrRows, size_t nrCols)
{
  MAP_INT4 *map;
  void *buf;
  PRECOND(TestApiInitTest("CreateSpatialINT4"));
  if (cr != CR_UINT1 && cr != CR_INT4 && cr != CR_REAL8 && cr != CR_REAL4)
        {
    Error("CreateSpatialINT4 cr must be UINT1,INT4,REAL4 or REAL8");
    return NULL;
  }
  if (cr == CR_REAL8)
      cr = CR_REAL4;
  buf = ChkMalloc( (size_t)(nrRows*nrCols*CELLSIZE(cr)));
  if (buf == NULL)
      return NULL;
  map = InitMapINT4(nrRows,nrCols, buf,TRUE, cr);
  if (map == NULL) {
      Free(buf);
      return NULL;
  }
  return map;
}
#line 1 "testapi.tem"
/* vim: syntax=c
 */

/* REAL8 */

MAP_REAL8 *ReadREAL8(
  const char *nameOrValue, size_t nrRows, size_t nrCols)
{
 REAL8 val;
 if (CnvrtREAL8(&val, nameOrValue))
  return InitMapREAL8(nrRows,nrCols,&val,FALSE,
                                                      CR_REAL8);
else
  {
   REAL8 dummy;
   if (CnvrtREAL8(&dummy, nameOrValue))
   { /* it's a number but not a valid one for REAL8
      */
     Error(" (in ReadREAL8) '%s' is not a valid REAL8",nameOrValue);
     return NULL;
   }
     return ReadREAL8Map(nameOrValue);
  }
}

MAP_REAL8 *ReadREAL8Map(const char *name)
{
  MAP *m;
        MAP_REAL8 *map;
        REAL8 max;
        UINT2 inCr;   /* in memory cellRepr */
        UINT2 fileCr; /* in file cellRepr */
        void *buf;

  PRECOND(name != NULL);
  m = Mopen(name, M_READ);
  if (m == NULL)
    MperrorExit(name,1);

  fileCr = RgetCellRepr(m);

  switch(fileCr) {
    case CR_INT1:
    case CR_INT2:
    case CR_UINT2:
    case CR_INT4:  inCr = CR_INT4; break;
    case CR_UINT1: inCr = CR_UINT1; break;
    case CR_REAL4:
    case CR_REAL8: inCr = CR_REAL4; break;
    case CR_UINT4:
    default      :
               Error(" '%s' has CR_UINT4 or illegal celRepr(%d)",
                    name, (int)fileCr);
               goto errExit;
        }
        if ( (CELLSIZE(inCr) > CELLSIZE(CR_REAL8))
              /* grabs all except: */ ||
             (inCr == CR_REAL4 && CR_REAL8 == CR_INT4)
           ) {
           Error(" (in ReadREAL8Map) '%s' is '%s' (too big for REAL8)",
                    name, RstrCellRepr(RgetCellRepr(m)));
    goto errExit; }

        if (RuseAs(m, inCr))
        {
          Error("RuseAs: Can't open '%s' as cellRepr '%d'\n",
                  name, (int)inCr);
          goto errExit;
        }
        Register(m);
        {
           size_t nrRows=RgetNrRows(m);
           size_t nrCols=RgetNrCols(m);
           /* first register then create */
           buf = ReadMapContents(m,  inCr,nrRows,nrCols);
          if (buf == NULL)
            goto errExit;
          map = InitMapREAL8(nrRows,nrCols,
                  buf,TRUE, inCr);
          if (map == NULL)
          {
            Free(buf);
            goto errExit;
          }
          RgetMaxVal(m, &max);
          map->maxVal = (int)max;
          map->valueScale = RgetValueScale(m);
        }
  return map;
errExit:
        Mclose(m);
        return NULL;
}

void WriteREAL8Map(const char *name,
                   MAP_REAL8 *map,
                   CSF_VS       valueScale)
{

  MAP *m;
  CSF_CR fileInCr = DetermineCellRepr(valueScale);
  /* fileInCr is different from m-?inCr */

  PRECOND(name != NULL);
  PRECOND(map != NULL);

  if ( fileInCr == CR_UINT2)
  {
    Error("(writing %s) valueScale is not type 2\n"
          " (probably inherited from an input map)", name);
    exit(1);
  }

  if (CELLSIZE(fileInCr) > CELLSIZE(CR_REAL8))
  {
   Error("(writing %s) valueScale does not support cellrepr REAL8\n",
    name);
   exit(1);
  }

  m = NewMap(name, fileInCr, valueScale);
  if (m == NULL)
    MperrorExit(name,1);
  if (RuseAs(m, map->inCellRepr))
    MperrorExit(name,1);
  RputSomeCells(m, 0, (UINT4)(map->nrRows*map->nrCols), map->spatialValue[0]);
  Mclose(m);
}

void DeleteMAP_REAL8(
  MAP_REAL8 *m)
{
  if (m->spatial)
    Free(m->spatialValue[0]);
  DeleteInternalMAP_REAL8(m);
}

MAP_REAL8 *CreateSpatialREAL8(CSF_CR cr,size_t nrRows, size_t nrCols)
{
  MAP_REAL8 *map;
  void *buf;
  PRECOND(TestApiInitTest("CreateSpatialREAL8"));
  if (cr != CR_UINT1 && cr != CR_INT4 && cr != CR_REAL8 && cr != CR_REAL4)
        {
    Error("CreateSpatialREAL8 cr must be UINT1,INT4,REAL4 or REAL8");
    return NULL;
  }
  if (cr == CR_REAL8)
      cr = CR_REAL4;
  buf = ChkMalloc( (size_t)(nrRows*nrCols*CELLSIZE(cr)));
  if (buf == NULL)
      return NULL;
  map = InitMapREAL8(nrRows,nrCols, buf,TRUE, cr);
  if (map == NULL) {
      Free(buf);
      return NULL;
  }
  return map;
}
