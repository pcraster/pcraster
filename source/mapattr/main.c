#include "stddefx.h" 

/*
 * mapattr.c 
 */


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <string.h>
#include "csf.h" 
#include "mathx.h" 
#include "misc.h" 
#include "app.h" 

/* global header (opt.) and mapattr's prototypes "" */
#include "mapattr.h"

/* headers of this app. modules called */ 
#include "mclone.h"

/***************/
/* EXTERNALS   */
/***************/


/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 


#define USAGE  \
 "USAGE: mapattr InputMaps\n" \
 "menu for clone creation if -e, -c, -p or -s are not specified\n" \
 "e   menu for changing location attributes of one map\n" \
 "c   copy location attributes of first map to others\n" \
 "p   print info of all maps\n" \
 "s   set attributes or create map without menu (see SET OPTIONS)\n" \
 "--clone f copy default values for clone creation\n"\
 "SET OPTIONS\n"\
 "R #     number of rows\n"\
 "C #     number of columns\n"\
 "BLNOSDV data type (default boolean)\n"\
 "--small\n"\
 "--large cell representation\n"\
 "x $     x coordinate upper left corner (default 0)\n"\
 "y $     y coordinate upper left corner (default 0)\n"\
 "l $     cell length (default 1)\n"\
 "P s     deprecated, projection, 'yb2t' (correct) or 'yt2b' (wrong)\n"\
 "a $     angle (default 0)\n"\
 "i #     file id (default 0)\n"\
 "EXPERIMENTAL\n"\
 "d       print data type in option format\n"

#define HEAD \
"attributes "

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/* in order of ATTR_defs !!
 */
static char const *printLabels[]={
"rows       ",
"columns    ",
"data_type  ",
"cell_repr  ",
"projection ",
"xUL        ",
"yUL        ",
"cell_length",
"angle(deg) ",
"file_id    ",
"min_val    ",
"max_val    ",
"version    ",
"native     ",
"attr_tab   "};

static BOOL printDataType=FALSE;

/******************/
/* IMPLEMENTATION */
/******************/

static void MergeOptAttr(
       ATTRIBUTES *a,
       const ATTRIBUTES *opt)
{
        if (opt->projection != PT_UNDEFINED)
          a->projection = opt->projection;
        if (opt->valueScale != VS_UNDEFINED)
          a->valueScale = opt->valueScale;
        if (opt->cellRepr != CR_UNDEFINED)
          a->cellRepr = opt->cellRepr;
        if (opt->nrRows != MV_UINT4)
          a->nrRows = opt->nrRows;
        if (opt->nrCols != MV_UINT4)
          a->nrCols = opt->nrCols;
        if (!(IS_MV_REAL8(&(opt->xUL))))
          a->xUL = opt->xUL;
        if (!(IS_MV_REAL8(&(opt->yUL))))
          a->yUL = opt->yUL;
        if (!(IS_MV_REAL8(&(opt->cellSize))))
          a->cellSize = opt->cellSize;
        if (!(IS_MV_REAL8(&(opt->angle))))
          a->angle = opt->angle;
        if (opt->gisFileId != MV_UINT4)
          a->gisFileId = opt->gisFileId;
        if (!(IS_MV_REAL8(&(opt->minVal))))
          a->minVal = opt->minVal;
        if (!(IS_MV_REAL8(&(opt->maxVal))))
          a->maxVal = opt->maxVal;
}

static void OptNotSetAttr( ATTRIBUTES *a)
{
       a->projection = PT_UNDEFINED;
       a->valueScale = VS_UNDEFINED;
       a->cellRepr = CR_UNDEFINED;
       a->nrRows = MV_UINT4;
       a->nrCols = MV_UINT4;
       SET_MV_REAL8(&(a->xUL));
       SET_MV_REAL8(&(a->yUL));
       SET_MV_REAL8(&(a->cellSize));
       SET_MV_REAL8(&(a->angle));
       a->gisFileId = MV_UINT4;
       SET_MV_REAL8(&(a->minVal));
       SET_MV_REAL8(&(a->maxVal));
}

static void DefaultAttr( ATTRIBUTES *a)
{
       a->version    = 2;
       a->projection = PT_YDECT2B;
       a->valueScale = VS_BOOLEAN;
       a->cellRepr = CR_UINT1;
       a->xUL = 0;
       a->yUL = 0;
       a->nrRows = MV_UINT4;
       a->nrCols = MV_UINT4;
       a->cellSize = 1;
       a->angle = 0;
       a->gisFileId = 0;
       SET_MV_REAL8(&(a->minVal));
       SET_MV_REAL8(&(a->maxVal));
}

static int ReadAttr(
       ATTRIBUTES *a,
       MAP *m,
       BOOL readOnly) /* are the attribute only used for teh PRINT op
                       */
{
       DefaultAttr(a);
       if (RuseAs(m, CR_REAL8))
              goto failure;
        RgetMinVal(m, &(a->minVal));
        RgetMaxVal(m, &(a->maxVal));
        a->projection = MgetProjection(m);
        a->xUL = RgetXUL(m);
        a->yUL = RgetYUL(m);
       a->nrRows = RgetNrRows(m);
       a->nrCols = RgetNrCols(m);
       a->cellSize = RgetCellSize(m);
        a->version    = MgetVersion(m);
       a->gisFileId = MgetGisFileId(m);
       a->byteOrder = m->main.byteOrder;
       a->attrTable = m->main.attrTable;
       if (Merrno)
              goto failure;
       if (a->version == 2 || readOnly)
       { /* otherwise use defaults */
          a->valueScale = RgetValueScale(m);
          a->cellRepr = RgetCellRepr(m);
         a->angle = RgetAngle(m);
         if (a->angle < 0)
             a->angle = -Rad2Deg(-a->angle);
         else
             a->angle = Rad2Deg(a->angle);
       }
       return 0;
failure:
       return 1;
}

static int CreateMap(
       const char *name,
       const ATTRIBUTES *a)
{
       UINT1 *buf;
       size_t i;
       double angle;
       MAP *m;

  size_t CELLMAX = (long int)(pow(2,30) - 1);
  if(((size_t)a->nrRows * (size_t)a->nrCols) > CELLMAX){
    printf("WARNING:\n  The specified amount of cells exceeds 2^30 - 1.\n  Not all PCRaster applications accept maps of this size.\n");
  }

       if (a->angle < 0)
           angle = -Deg2Rad(-a->angle);
       else
           angle = Deg2Rad(a->angle);
       m = Rcreate(name,(size_t)a->nrRows,(size_t)a->nrCols, a->cellRepr, a->valueScale,
                     a->projection, a->xUL, a->yUL, angle, a->cellSize);
       if (m == NULL)
        goto error1;

       PRECOND(a->gisFileId != MV_UINT4);
       if (MputGisFileId(m,a->gisFileId) == MV_UINT4)
              goto error2;

       if (RuseAs(m, CR_UINT1))
              goto error2;
       buf = (UINT1 *)Rmalloc(m, (size_t)a->nrCols);
       if (buf == NULL)
       {
        Mclose(m);
        remove(name);
        return 1;
       }
       for(i=0; i < a->nrRows; i++)
       {
              memset(buf,1,(size_t)a->nrCols);
              RputRow(m,i,buf);
       }
       Free(buf);
       Mclose(m);
       return 0;
error2:
       Mclose(m);
       remove(name);
error1:
       return       RetError(1,"Can not create '%s': %s",name,MstrError());
}

static int SetAndCloseMap(
       MAP *m,
       const ATTRIBUTES *a)
{
       double angle;

       if (! (IS_MV_REAL8(&(a->angle))))
       {
        if (a->angle < 0)
           angle = -Deg2Rad(-a->angle);
        else
           angle = Deg2Rad(a->angle);
       }
       if (RuseAs(m, CR_REAL8))
              goto error2;

       if (a->projection != PT_UNDEFINED)
        MputProjection(m,a->projection);
       if (! (IS_MV_REAL8(&(a->xUL))))
        RputXUL(m, a->xUL);
       if (! (IS_MV_REAL8(&(a->yUL))))
        RputYUL(m, a->yUL);
       if (! (IS_MV_REAL8(&(a->angle))))
        RputAngle(m, angle);
       if (! (IS_MV_REAL8(&(a->cellSize))))
        RputCellSize(m, a->cellSize);
       if(a->gisFileId != MV_UINT4)
        MputGisFileId(m,a->gisFileId);

       if (Merrno)
              goto error2;
       return 0;
error2:
       return       RetError(1,"Can not write to '%s': %s",MgetFileName(m),MstrError());
}

static int DefaultCloneAttr(
       ATTRIBUTES *a)
{
       MAP *in;
       if (appClone != NULL)
       {
           char *dummy;
           in = AppOpenClone(&dummy, NULL);
           if (in == NULL )
                  return 1;
           if (ReadAttr(a,in,FALSE))
           {
                  Mclose(in);
                  return RetError(1,"while reading clone map '%s': %s",appClone,MstrError());
           }
           Mclose(in);
       }
       else
           DefaultAttr(a);
       return 0;
}

static int PrintOption(
 const char **names,
 int nrNames)
{
       ATTRIBUTES *a = (ATTRIBUTES *)ChkMalloc(sizeof(ATTRIBUTES) * nrNames);
       int *colLen = (int *)ChkMalloc(sizeof(int) * nrNames);
       int i;
       if (a == NULL || colLen == NULL)
              return 1;
       for(i = 0 ; i < nrNames; i++)
       {
              MAP *m= Mopen(names[i],M_READ);
              if (m == NULL)
                  {
                   Free(a);
                   return RetError(1,"while reading map '%s': %s",names[i],MstrError());
                  }
                  ReadAttr(a+i,m,TRUE);
                  colLen[i] = MAX(11, strlen(names[i]));
                  Mclose(m);
       }

       if (printDataType)
       {
        for(i = 0 ; i < nrNames; i++)
        {
        int c;
         switch(a[i].valueScale) {
              case VS_LDD : c = 'L'; break;
              case VS_SCALAR : c = 'S'; break;
              case VS_BOOLEAN : c = 'B'; break;
              case VS_NOMINAL : c = 'N'; break;
              case VS_ORDINAL : c = 'O'; break;
              case VS_DIRECTION : c = 'D'; break;
              default : c = ' '; break;
         }
         printf("%c",c);
        }
        return 1;
       }

        printf("%s",HEAD);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*s", colLen[i], names[i]);
       printf("\n");

        printf("%s",printLabels[ATTR_nrRows]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*u", colLen[i], a[i].nrRows);
       printf("\n");

        printf("%s",printLabels[ATTR_nrCols]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*u", colLen[i], a[i].nrCols);
       printf("\n");

        printf("%s",printLabels[ATTR_cellSize]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*g", colLen[i], a[i].cellSize);
       printf("\n");

       
        printf("%s",printLabels[ATTR_valueScale]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*s", colLen[i], RstrValueScale(a[i].valueScale));
       printf("\n");

        printf("%s",printLabels[ATTR_cellRepr]);
       for(i = 0 ; i < nrNames; i++)
       { const char *cStr;
         switch(a[i].cellRepr)
         { case CR_UINT1 : cStr = "small"; break;
           case CR_INT4  : cStr = "large"; break;
           case CR_REAL4 : cStr = "single"; break;
           case CR_REAL8 : cStr = "double"; break;
           default : cStr = RstrCellRepr(a[i].cellRepr);
          }
         printf(" %-*s", colLen[i], cStr);
        }
       printf("\n");

       printf("%s",printLabels[ATTR_projection]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*s", colLen[i], a[i].projection ? "yb2t" : "yt2b" );
       printf("\n");

        printf("%s",printLabels[ATTR_angle]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*g", colLen[i], a[i].angle);
       printf("\n");

       printf("%s",printLabels[ATTR_xUL]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*g", colLen[i], a[i].xUL);
       printf("\n");

       printf("%s",printLabels[ATTR_yUL]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*g", colLen[i], a[i].yUL);
       printf("\n");

       printf("%s",printLabels[ATTR_minVal]);
       for(i = 0 ; i < nrNames; i++)
        if (IS_MV_REAL8(&(a[i].minVal)))
         printf(" %-*s", colLen[i], "mv");
        else
        { 
         if ((a[i].cellRepr) & CSF_FLOAT_MASK)
          printf(" %-*g", colLen[i], a[i].minVal);
         else
          printf(" %-*d", colLen[i], (int)a[i].minVal);
        }
       printf("\n");

       printf("%s",printLabels[ATTR_maxVal]);
       for(i = 0 ; i < nrNames; i++)
        if (IS_MV_REAL8(&(a[i].maxVal)))
         printf(" %-*s", colLen[i], "mv");
        else
        {
         if ((a[i].cellRepr) & CSF_FLOAT_MASK)
          printf(" %-*g", colLen[i], a[i].maxVal);
         else
          printf(" %-*d", colLen[i], (int)a[i].maxVal);
        }
       printf("\n");

       printf("%s",printLabels[ATTR_version]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*u", colLen[i], a[i].version);
       printf("\n");

       printf("%s",printLabels[ATTR_gisFileId]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*u", colLen[i], a[i].gisFileId);
       printf("\n");

       printf("%s",printLabels[ATTR_byteOrder]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*s", colLen[i], a[i].byteOrder == 1 ? "y" : "n");
       printf("\n");

       printf("%s",printLabels[ATTR_attrTable]);
       for(i = 0 ; i < nrNames; i++)
         printf(" %-*s", colLen[i], a[i].attrTable == 0 ? "n" : "y");
       printf("\n");

       return 0;
}

static int CloneOption(
       const char *name)
{
       ATTRIBUTES a;
       if (FileStat(name) != 2)
       {
          return RetError(1,"file '%s' exists, give new (non-existing) name",name);
       }
       if (DefaultCloneAttr(&a))
              return 1;
       a.cloneCreation = TRUE;
        switch(MakeCloneMenu(&a, name))
        { 
            case 0: return 1;
            case 1: return CreateMap(name, &a);
            case 2: fprintf(stderr,"No map created\n");
                    return 0;
       }
        POSTCOND(FALSE);
        return 1;
}

static int EditOption(
       const char *name)
{
       ATTRIBUTES a;
       MAP *in = Mopen(name, M_READ_WRITE);
       if (in == NULL || ReadAttr(&a,in,FALSE))
       {
         if (in != NULL)
          Mclose(in);
         return RetError(1,"while reading '%s': %s",name,MstrError());
       }
       if (a.version != 2)
        return RetError(1,"'%s' is not a version 2 map, no edits possible");
       a.cloneCreation = FALSE;
        switch(MakeCloneMenu(&a, name))
        { 
            case 0: return 0;
            case 1: return SetAndCloseMap(in, &a);
            case 2: fprintf(stderr,"No map attributes written\n");
                    return 0;
        }
        POSTCOND(FALSE);
        return 1;
}

static MAP **OpenCopyOrSetMaps(
        ATTRIBUTES *a,   /* attributes first one */
       BOOL      copy,   /* copy or set ? 
                          * if set then first map opened read write
                          * read otherwise
                          */
       const char **names,
       int   nrNames)
{
       MAP **maps;
       int i;
       maps = (MAP **)ChkCalloc((size_t)nrNames, sizeof(MAP *));
       if (maps == NULL)
              return NULL;

       for(i=0; i < nrNames; i++)
       {
        maps[i] = Mopen(names[i],
                    (i==0 && copy) ? M_READ : M_READ_WRITE);
        if (maps[i] == NULL)
        {
         Error("while reading '%s': %s",names[i],MstrError());
         goto error;
        }
        if (i == 0)
               ReadAttr(a,maps[0],FALSE);
        if (MgetVersion(maps[i]) != 2)
        {
         Error("'%s' is not a version 2 map, no %s possible", names[i],
               copy ? "copy" : "set");
         goto error;
        }
        if (RgetNrRows(maps[i]) != a->nrRows 
         || RgetNrCols(maps[i]) != a->nrCols)
        {
         Error("number of rows and columns of '%s' do not match '%s'",
               names[i],names[0]);
         goto error;
        }
       }
       return maps;
error:
       for(i=0;i<nrNames; i++)
              if (maps[i] != NULL)
                     Mclose(maps[i]);
        Free(maps);
        return NULL;
}


static int CopyOption(
       const char **names,
       int   nrNames)
{
       ATTRIBUTES a;
       MAP **maps;
       int i;

        maps = OpenCopyOrSetMaps(&a,TRUE,names,nrNames);
       if (maps == NULL)
              return 1;
       for(i=1; i < nrNames; i++)
           SetAndCloseMap(maps[i], &a);
        Free(maps);
        return 0;
}

static int SetOption(
       const char **names,
       int   nrNames,
       const ATTRIBUTES *opt)
{
       ATTRIBUTES a;
       MAP **maps;
       int i;

       if (DefaultCloneAttr(&a))
              return 1;

       if (FileStat(names[0]) == 2)
       { /* non-existing */
          MergeOptAttr(&a, opt);
          if (nrNames > 1)
           return RetError(1,"-s: only one new map or multiple existing maps allowed");
          if (a.nrRows == MV_UINT4 || a.nrCols == MV_UINT4)
           return RetError(1,"-s: -R and/or -C not specified");
           return CreateMap(names[0], &a);
       }
       else
       { /* set all */
         if (opt->nrRows != MV_UINT4 || opt->nrCols != MV_UINT4)
           return RetError(1,"-s: can not change number of rows or columns of an existing map");
         if (opt->valueScale != VS_UNDEFINED)
           return RetError(1,"-s: can not change data type of an existing map");
          maps = OpenCopyOrSetMaps(&a,FALSE,names,nrNames);
         if (maps == NULL)
              return 1;
         for(i=0; i < nrNames; i++)
           SetAndCloseMap(maps[i], opt);
          Free(maps);
          return 0;
        }
}

int main(int argc,                     /* number of arguments */
       char *argv[])                     /* list of arguments */
{
       ATTRIBUTES opt;
       int c;
        CSF_VS valueScale = VS_UNDEFINED;
       enum MODE { CLONE, EDIT, COPY, PRINT, SET } mode = CLONE;

       /* install application */
       if(InstallArgs(argc, argv, "(ecps)dR#C#i#P*x$y$l$a$(BLNOSDV)", 
          "mapattr", __DATE__))
              goto failure;
         OptNotSetAttr(&opt);

       while((c = GetOpt()) != 0)
       {
        switch(c)
        {
         case 'd': printDataType = TRUE; mode = PRINT; break;
         case 'e': mode = EDIT; break;
         case 'c': mode = COPY; break;
         case 'p': mode = PRINT; break;
         case 's': mode = SET; break;
         case 'R': 
            if (*((const int *)OptArg) <= 0)
            { Error("-R number of rows must be greater than 0 (not '%d)",
                     *((const int *)OptArg));
              goto failure;
            }
            opt.nrRows = *((const int *)OptArg); 
            break;
         case 'C': 
            if (*((const int *)OptArg) <= 0)
            { Error("-R number of columns must be greater than 0 (not '%d)",
                     *((const int *)OptArg));
              goto failure;
            }
            opt.nrCols = *((const int *)OptArg); 
            break;
         case 'i': 
            if (*((const int *)OptArg) < 0)
            { Error("-i file id must be >= 0 (not '%d)",
                     *((const int *)OptArg));
              goto failure;
            }
            opt.gisFileId = *((const int *)OptArg); 
            break;
         case 'P':
             if (StrEq("yt2b", (const char *)OptArg))
             {
                    opt.projection = PT_YINCT2B;
                    break;
             }
             if (StrEq("yb2t", (const char *)OptArg))
             {
                    opt.projection = PT_YDECT2B;
                    break;
             }
            Error("-P unknown projection '%s'",(const char *)OptArg);
            goto failure;
         case 'x': opt.xUL = *((const double *)OptArg); break;
         case 'y': opt.yUL = *((const double *)OptArg); break;
         case 'l': 
            if (*((const double *)OptArg) <= 0)
            { Error("-l cell length must be greater than 0 (not '%g)",
                     *((const double *)OptArg));
              goto failure;
            }
            opt.cellSize = *((const double *)OptArg); break;
         case 'a': 
            if ( (*((const double *)OptArg) < -90 )
                || (*((const double *)OptArg) > 90 ) )
            { Error("-a angle must be between -90 and 90 (not '%g)",
                     *((const double *)OptArg));
              goto failure;
            }
            opt.angle = *((const double *)OptArg); break;
#         include "case_vs.h"
       } /* eoswitch */
       } /* eowhile */
       if (valueScale != VS_UNDEFINED)
       {
         opt.valueScale = valueScale;
               opt.cellRepr = AppDefaultCellRepr(valueScale);
       }

       if ( (argv = ArgArguments(&argc)) == NULL)
              goto failure;

       if (AppArgCountCheck(argc,2,-1,USAGE))
              goto failure;

       switch(mode) {
        case PRINT: 
                    if (PrintOption((const char **)argv+1,argc-1))
                           goto failure;
                    break;
        case CLONE: if (argc != 2)
                    {
                     Error("Too many arguments, only one map allowed");
                     goto failure;
                    }
                    if (CloneOption(argv[1]))
                           goto failure;
                    break;
        case EDIT:  if (argc != 2)
                    {
                     Error("-e: Too many arguments, only one map allowed");
                     goto failure;
                    }
                    if (EditOption(argv[1]))
                           goto failure;
                    break;
       case COPY:
                    if (argc <= 2)
                    {
                     Error("-c: requires more than one map");
                     goto failure;
                    }
                    if (CopyOption((const char **)argv+1,argc-1))
                           goto failure;
                    break;
        case SET:
                    if (SetOption((const char **)argv+1,argc-1,&opt))
                           goto failure;
                    break;
       }

       AppEnd();
       exit(0);
       return 0;

failure:
       AppEnd();
       exit(1);
       return 1;
} /* main */
