#include "stddefx.h"

#ifndef INCLUDED_CALC_ESRIGRIDIO
#include "calc_esrigridio.h"
#define INCLUDED_CALC_ESRIGRIDIO
#endif

#ifndef INCLUDED_COM_DYNAMICLIBRARY
#include "com_dynamiclibrary.h"
#define INCLUDED_COM_DYNAMICLIBRARY
#endif

#ifdef WIN32
#ifndef INCLUDED_COM_WIN32REGISTRYKEY
#include "com_win32registrykey.h"
#define INCLUDED_COM_WIN32REGISTRYKEY
#endif
#ifndef INCLUDED_COM_WIN32
#include "com_win32.h"
#define INCLUDED_COM_WIN32
#endif
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

#ifndef INCLUDED_GIOAPI
#include "gioapi.h"
#define INCLUDED_GIOAPI
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

com::DynamicLibrary *calc::EsriGridIO::d_dll(0);
std::string calc::EsriGridIO::d_dllName;

#define STATIC_GRIDIO_FPTR(funcName) \
  STATIC_DLL_FUNC_PTR(d_dll,funcName)

typedef int (*IntVoidPtr)(void);
bool esriArcView3Only=false;

#ifdef WIN32
//! try opening one of the Dll's with current PATH setting
static com::DynamicLibrary* openInPathDll()
{
  com::DynamicLibrary *dll(0);

  const char *names[2]= {"aigridio","avgridio"};
  size_t i= esriArcView3Only ? 1 : 0;
  for (; i < 2; ++i) {
   try {
     dll = new com::DynamicLibrary(names[i]);
   } catch (...) {
   }
   if (dll) {
     calc::EsriGridIO::d_dllName = names[i];
     return dll;
   }
  }
  return 0;
}

static com::DynamicLibrary* openGridDll(
    const std::string& dllDir) {
// open dll by PATH injection, needed to also load the
//  depend dll's such as avfeat.dll
  com::DynamicLibrary *dll(0);

  const char *names[2]= {"aigridio.dll","avgridio.dll"};
  for (size_t i=0; i < 2; ++i) {
   com::PathName dllPathName(dllDir);
   dllPathName+=names[i];
   if (com::exists(dllPathName)) {
       char oldPath[16001];
       GetEnvironmentVariable("PATH",oldPath,16000);
       std::string newPath(dllDir);
       newPath+=";";
       newPath+=oldPath;
       BOOL success=SetEnvironmentVariable("PATH",newPath.c_str());
       POSTCOND(success);
       dll = openInPathDll();
       if (dll)
         return dll;
   }
  }
  return 0;
}

static com::DynamicLibrary* openGridDll() {

  com::DynamicLibrary *dll(0);
  // try with current PATH settings:
  dll = openInPathDll();
  if (dll)
    return dll;
if (!esriArcView3Only) {
  // ArcGis 9 tested
  {
   com::Win32RegistryKey k(com::Win32RegistryKey::LocalMachine,
      "SOFTWARE\\ESRI\\ArcGIS");
   if(k.exists()) {
      dll= openGridDll(k.value("InstallDir")+"bin");
      if (dll)
        return dll;
   }
  }

  // ArcGis 8(.3) guesses
  {
   com::Win32RegistryKey k(com::Win32RegistryKey::LocalMachine,
      "SOFTWARE\\ESRI\\ArcInfo\\Desktop\\8.0");
   if (k.exists()) {
     dll= openGridDll(k.value("InstallDir")+"bin");
     if (dll)
        return dll;
   }
  }
  // ArcGis 8(.3) guesses
  {
   com::Win32RegistryKey k(com::Win32RegistryKey::LocalMachine,
      "SOFTWARE\\ESRI\\ArcInfo\\Workstation\\8.0");
   if (k.exists()) {
     dll= openGridDll(k.value("InstallDir")+"bin");
     if (dll)
        return dll;
   }
  }
}
  // ArcView 3.X tested
  {
   com::Win32RegistryKey k(com::Win32RegistryKey::LocalMachine,
      "SOFTWARE\\ESRI\\ArcView Spatial Analyst\\CurrentVersion");
   if(k.exists()) {
      dll= openGridDll(k.value("Path"));
      if (dll)
       return dll;
   }
  }
  return 0;
}
#endif

/*!
 \todo
   make a difference in fail between
    - lib not found, is ArcView installed?
    - sym not found, do we have correct version of av-dll
*/
calc::EsriGridIO::EsriGridIO()
{
  try {
    if (!d_dll) {
#ifdef WIN32
/*
 *   // insert PATH to avgridio (an dependants) as first in path
 *   com::Win32RegistryKey k(com::Win32RegistryKey::LocalMachine,
 *    "SOFTWARE\\ESRI\\ArcView Spatial Analyst\\CurrentVersion");
 *   if(!k.exists())
 *     throw std::runtime_error("ArcView Spatial Analyst not installed");
 *   std::string path(k.value("Path")+";");
 *   if (path==";")
 *     throw std::runtime_error("ArcView Spatial Analyst bad registry settings");
 *   char oldPath[16001];
 *   GetEnvironmentVariable("PATH",oldPath,16000);
 *   path+=oldPath;
 *   BOOL success=SetEnvironmentVariable("PATH",path.c_str());
 *   POSTCOND(success);
 */
     d_dll= openGridDll();
#else 
     d_dll = new com::DynamicLibrary("avgridio");
#endif
     if (!d_dll)
       throw 
        com::Exception("No correct version of Spatial Analyst or ArcGrid found");
    }
    if(!d_dll->wasAlreadyLoaded()) {
     IntVoidPtr gridIOSetup = (IntVoidPtr)
       d_dll->loadFunction("GridIOSetup");
     (*gridIOSetup)();
    }
  } catch (...) {
    clean();
    throw;
  }
}

void calc::EsriGridIO::throwError(const std::string msg)
{
   std::ostringstream str;
   str << msg << std::endl;
   str << std::endl;
   str << "Extended info:" << std::endl;
   str << "dll: " << d_dllName << std::endl;
   str << "cwd: " << com::currentWorkingDirectory().toString() << std::endl;
#ifdef WIN32
   str << "win32: " << com::win32GetLastError() << std::endl;
#endif
   throw std::runtime_error(str.str());
}

void calc::EsriGridIO::clean()
{
  if(d_dll && !d_dll->wasAlreadyLoaded()) {
   // should load symbol on init to verify existence !
   IntVoidPtr gridIOExit = (IntVoidPtr)d_dll->loadFunction("GridIOExit");
   (*gridIOExit)();
  }
  delete d_dll;
  d_dll=0;
}

calc::EsriGridIO::~EsriGridIO()
{
  clean();
}


bool calc::EsriGridIO::gridExists(const std::string& gridName)
{
  typedef int (*T_GridExists)(const char *);
  STATIC_GRIDIO_FPTR(GridExists);
  return (*funcPtr)(gridName.c_str()) != 0;
}

#ifdef WIN32

#ifndef INCLUDED_COM_WIN32
#include "com_win32.h"
#define INCLUDED_COM_WIN32
#endif

static void checkWin32Error(bool succes=false)
{
 if (!succes)
  throw std::runtime_error(com::win32GetLastError());
}


DWORD WINAPI CloseDialog(LPVOID /* notUsed */) {
 DWORD  dwResult=0;

 HWND   win;
 const char *signature="x_x_Press cancel";
 size_t len = strlen(signature);
 char cap[512];

 while (1) {
   win = GetForegroundWindow();
   if (!GetWindowText(win,cap,500))
     return dwResult; // bad luck this will crash: checkWin32Error();
   if (!strncmp(cap, signature, len))
     break;
   // wait 'till something is changed in the main thread
   DWORD r = WaitForInputIdle(GetCurrentProcess(),4000);
   if (r == WAIT_TIMEOUT) // 4 secs elapsed
    return dwResult; // something wrong but do not hang!
 }

 // DestroyWindow(win); allas is not intra-thread
 SendMessage(win,WM_CLOSE,0,0);

 return dwResult;
}

static void FixCancel()
{
 DWORD closeId;
 HANDLE hClose  = CreateThread(NULL,0,CloseDialog, NULL,0, &closeId);
 checkWin32Error(hClose != NULL);

 // wrap with a cach ArcGis does this not have AvExec I think
 try {
 com::DynamicLibrary avExec("avexec32");
 typedef char *(*T_AVExec)(char *cmd);
 T_AVExec e=(T_AVExec)avExec.loadFunction("AVExec");
 (*e)("av.Run(\"PCRaster.FixFatProblem\",nil)");
 } catch(...) {
 }
}

#endif

void  calc::EsriGridIO::gridDelete(const std::string& gridName)
{
// int GridKill(ConstCstr grid_name, int option /* 0,1 does something */);
// int CellLyrDelete (ConstCstr clyr_name);
// int GridDelete(ConstCstr clyr_name);

#ifdef WIN32
  // this a patch to check if AV keeps the VAT
  // errousnaly open
  com::PathName pn(gridName);
  pn.makeAbsolute();
  pn+="vat.adf";
  com::PathInfo pi(pn);
  if (pi.exists()) {
    HANDLE h = CreateFile(pn.toString().c_str(),
           GENERIC_READ|GENERIC_WRITE,0,NULL,
           OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
    if (h == INVALID_HANDLE_VALUE) {
      FixCancel();

/* NON THREAD BASED CODE
      com::DynamicLibrary avExec("avexec32");
      // discern between calling from AV and cmdLine
      //  still have to test CW wasAlreadyLoaded does not work correctly
      // if (avExec.wasAlreadyLoaded()) {
 ************************ */
    }
    if (h != INVALID_HANDLE_VALUE)
       CloseHandle(h);
  }
#endif
  // end of patch
  typedef int (*T_GridDelete)(const char *);
  STATIC_GRIDIO_FPTR(GridDelete);
  if  ( (*funcPtr)(gridName.c_str()) < 0 )
   throwError("Esri GridDelete failed on "+gridName);
}

void calc::EsriGridIO::bndCellRead(const std::string& grdnam,
    double *box)
{
  typedef int
     (*T_BndCellRead)(const char *name, double *box);
  STATIC_GRIDIO_FPTR(BndCellRead);
  if  ( (*funcPtr)(grdnam.c_str(),box) < 0 )
   throwError("Esri BndCellRead failed");
}

//! see implementation, has major problems!
void calc::EsriGridIO::describeGridDbl(
  const std::string& grdnam, double *cellsz, int *gridsz,
                    double *box, double *sta,
                    bool& isFloat, int *nclass, int *reclen)
{
  // stumbled on a lot of problems, with ArcView 3.1
  // this call prints Error in determining Grid Size
  //  when x=0 is not in the map e,g:
  // ncols         492
  // nrows         657
  // xllcorner     -1425000
  // yllcorner     -282000
  // cellsize      1000    x0 = -1425000 < 492*1000 o.i.d.
  // and the gridsizes are incorrect
  // d_nrRows = static_cast<size_t>(gridSize[ROW_GRIDSIZE]);
  // d_nrCols = static_cast<size_t>(gridSize[COL_GRIDSIZE]);
  // bool isFloat;
  // const size_t MIN_STA=0;
  // const size_t MAX_STA=1;
  // double sta[5] = { -1,-1,-1,-1,-1}; // make sure we have values if all mv
  // calc::EsriGridIO::describeGridDbl(fileName,
  //    &d_cellSize, gridSize, d_box,sta, isFloat, &nclass, &reclen);
  typedef int
     (*T_DescribeGridDbl)(const char *name, double *cellSize, int *,
                    double *, double *, int *, int *, int *);
  STATIC_GRIDIO_FPTR(DescribeGridDbl);
  int dataTypeInt;
  if ((*funcPtr)
      (grdnam.c_str(), cellsz,gridsz, box, sta, &dataTypeInt, nclass,reclen)
     ) {
    ; // throwError("Esri DescribeGrdDbl failed");
  }

  //! patch! above call does puts cellsize in box[0] !
  bndCellRead(grdnam.c_str(), box);
  switch (dataTypeInt) {
   case CELLINT  : isFloat = false; break;
   case CELLFLOAT: isFloat = true ; break;
   default: throwError("esri violates its api");
  }
}

//! an internal block size (not nrows/ncols), not interesting
void calc::EsriGridIO::cellLyrBlockSize(
  int channel,
  int *bxcells,
  int *bycells)
{
  typedef int (*T_CellLyrBlockSize)(
    int channel, int *bxcells, int *bycells);
  STATIC_GRIDIO_FPTR(CellLyrBlockSize);
  if ((*funcPtr)(channel, bxcells, bycells) < 0)
     throwError("esrigrid CellLyrBlockSize");
}

//! return channel id
int calc::EsriGridIO::cellLayerOpen(
  const std::string& grdnam,
  int rdwrflag, int iomode,
  int *celltype, double *cellsize)
{
  typedef int (*T_CellLayerOpen)(
    const char* grdnam,
    int rdwrflag, int iomode,
    int *celltype, double *cellsize);
  STATIC_GRIDIO_FPTR(CellLayerOpen);
  int chanId = (*funcPtr)
    (grdnam.c_str(), rdwrflag, iomode, celltype, cellsize);
  if (chanId < 0)
   throwError("esrigrid CellLayerOpen");
  return chanId;
}

void calc::EsriGridIO::cellLyrClose(int chanId)
{
  PRECOND(chanId >= 0);
  typedef int (*T_CellLyrClose)(int channel);
  STATIC_GRIDIO_FPTR(CellLyrClose);
  if ( (*funcPtr)(chanId) < 0 )
    throwError("esrigrid CellLayerClose");
}

void calc::EsriGridIO::cellLyrCloseNoVat(int chanId)
{
  PRECOND(chanId >= 0);
  typedef int (*T_CellLyrCloseNoVat)(int channel);
  STATIC_GRIDIO_FPTR(CellLyrCloseNoVat);
  if ( (*funcPtr)(chanId) < 0 )
    throwError("esrigrid CellLayerCloseNoVat");
}

void calc::EsriGridIO::getWindowBandFloat(
  int chanId, int startrow, int nrRows,  float **ptrVal)
{
  typedef int (*T_GetWindowBandFloat)(
    int d_chanId, int startrow,
    int nrRows,  float **ptrVal);
  STATIC_GRIDIO_FPTR(GetWindowBandFloat);
  if ((*funcPtr)(chanId, startrow, nrRows, ptrVal) < 0)
   throwError("esrigrid GetWindowBandFloat");
}

void calc::EsriGridIO::getWindowBandInt(
  int chanId, int startrow, int nrRows,  int **ptrVal)
{
  typedef int (*T_GetWindowBandInt)(
    int d_chanId, int startrow,
    int nrRows,  int **ptrVal);
  STATIC_GRIDIO_FPTR(GetWindowBandInt);
  if ((*funcPtr)(chanId, startrow, nrRows, ptrVal) < 0)
   throwError("esrigrid GetWindowBandInt");
}

void calc::EsriGridIO::putWindowBand(
    int channel, int startrow, int nrows, const void * const * bandbuf)
{
       typedef int (*T_PutWindowBand)
    (int channel, int startrow, int nrows, CELLTYPE **bandbuf);
  STATIC_GRIDIO_FPTR(PutWindowBand);
  if ((*funcPtr)(channel,startrow,nrows, (CELLTYPE **)bandbuf) < 0)
     throwError("esrigrid PutWindowBand");
}

void calc::EsriGridIO::getMissingFloat(
    float *mvVal)
{
  typedef void (*T_GetMissingFloat)( float *mvVal);
  STATIC_GRIDIO_FPTR(GetMissingFloat);
  (*funcPtr)(mvVal);
}

int calc::EsriGridIO::privateWindowCols(
  int chan_id)
{
  typedef int (*T_PrivateWindowCols)(int chan_id);
  STATIC_GRIDIO_FPTR(PrivateWindowCols);
  return (*funcPtr)(chan_id);
}

int calc::EsriGridIO::privateWindowRows(
  int chan_id)
{
  typedef int (*T_PrivateWindowRows)(int chan_id);
  STATIC_GRIDIO_FPTR(PrivateWindowRows);
  return (*funcPtr)(chan_id);
}

void calc::EsriGridIO::privateWindowBox(
  int chan_id,
  double box[4])
{
  typedef int (*T_PrivateWindowBox)(int chan_id,
    double box[4]);
  STATIC_GRIDIO_FPTR(PrivateWindowBox);
  if ((*funcPtr)(chan_id, box) < 0 )
     throwError("esrigrid PrivateWindowBox");
}

void calc::EsriGridIO::privateAccessWindowSet(
  int chan_id,
  const double bndBox[4], double cellSize, double adjBndBox[4],
  size_t& nrResRows, size_t& nrResCols)
{
  typedef int (*T_PrivateAccessWindowSet)(int chan_id,
    const double bndBox[4], double cellSize, double adjBndBox[4]);
  STATIC_GRIDIO_FPTR(PrivateAccessWindowSet);
  if ((*funcPtr)(chan_id, bndBox, cellSize, adjBndBox) < 0 )
     throwError("esrigrid PrivateAccessWindowSet 1");

  int c = privateWindowRows(chan_id);
  if ( c <= 0 )
     throwError("esrigrid PrivateAccessWindowSet 2");
  nrResRows = static_cast<size_t>(c);

  c = privateWindowCols(chan_id);
  if ( c <= 0 )
     throwError("esrigrid PrivateAccessWindowSet 3");
  nrResCols = static_cast<size_t>(c);
}


int  calc::EsriGridIO::cellLayerCreate (
    const std::string& name,
    int rdwrflag,
    int iomode,
                int celltype, double cellsize, const double box[4])
{
  typedef int (*T_CellLayerCreate)(
    const char* name,
    int rdwrflag,
    int iomode,
    int celltype, double cellsize, const double box[4]);
  STATIC_GRIDIO_FPTR(CellLayerCreate);

  int chan_id = (*funcPtr)(
    name.c_str(), rdwrflag, iomode, celltype,
    cellsize, box);
  if (chan_id < 0 )
     throwError("esrigrid CellLayerCreate "+name);
  return chan_id;
}

//!  get min and max value of grid
/*!
     \exception std::runtime_error if all mv's
 */
void calc::EsriGridIO::StaGetMinmaxDbl(
    const std::string& fileName,
    double *min,
    double *max)
{
  typedef int (*T_StaGetMinmaxDbl)(const char* name, double *min, double *max);
  STATIC_GRIDIO_FPTR(StaGetMinmaxDbl);

  if ((*funcPtr)(fileName.c_str(), min, max) < 0)
     throwError("esrigrid StaGetMinmaxDbl");
}

void calc::EsriGridIO::putWindowCellFloat(
  int channel,
  size_t col,
  size_t row,
  double fcell)
{
  // prototypw in gioaopi.h says
  // int PutWindowCellFloat(int channel, int col, int row, double fcell);
  // but it is:
  typedef int (*T_PutWindowCellFloat)( int channel,
          int col, int row, float fcell);
  STATIC_GRIDIO_FPTR(PutWindowCellFloat);
  if ((*funcPtr)(channel,
            static_cast<int>(col),static_cast<int>(row),
      static_cast<float>(fcell)) < 0)
     throwError("esrigrid PutWindowCellFloat");
}

void calc::EsriGridIO::putWindowCellInt(
  int channel,
  size_t col,
  size_t row,
  int icell)
{
  typedef int (*T_PutWindowCellInt)( int channel, int col, int row, int icell);
  STATIC_GRIDIO_FPTR(PutWindowCellInt);

  if ((*funcPtr)(channel,static_cast<int>(col),static_cast<int>(row),icell) < 0)
     throwError("esrigrid PutWindowCellInt");
}

void calc::EsriGridIO::privateAccessWindowClear(int channel)
{
  typedef int (*T_PrivateAccessWindowClear)( int channel);
  STATIC_GRIDIO_FPTR(PrivateAccessWindowClear);

  if ((*funcPtr)(channel) < 0)
     throwError("esrigrid privateAccessWindowClear");
}

void calc::EsriGridIO::cellLyrSta (int channel, double *dmin, double *dmax,
                double *dmean, double *dstdv)
{
  typedef int (*T_CellLyrSta)(int channel, double *dmin, double *dmax,
                double *dmean, double *dstdv);
  STATIC_GRIDIO_FPTR(CellLyrSta);
  if ((*funcPtr)(channel, dmin, dmax, dmean, dstdv) < 0)
     throwError("esrigrid cellLayerSta");
}
