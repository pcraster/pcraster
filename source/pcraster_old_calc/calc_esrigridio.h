#ifndef INCLUDED_CALC_ESRIGRIDIO
#define INCLUDED_CALC_ESRIGRIDIO

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_COM_DYNAMICLIBRARY
#include "com_dynamiclibrary.h"
#define INCLUDED_COM_DYNAMICLIBRARY
#endif

namespace calc {

class EsriGridIO {
private:
  static com::DynamicLibrary *d_dll;
  void   clean();
public:
  static std::string          d_dllName;

  EsriGridIO();
 ~EsriGridIO();

  static void throwError(const std::string msg);

  // ACCESSORS
  static bool gridExists(const std::string& gridName);
  static void describeGridDbl(const std::string& grdnam, 
    double *cellsz, int *gridsz, 
                double *box, double *sta, 
                bool& isFloat, int *nclass, int *reclen);
  static void bndCellRead(const std::string& grdnam, 
    double *box);
  static int cellLayerOpen(
    const std::string& grdnam, 
    int rdwrflag, int iomode,
    int *celltype, double *cellsize);
  static void cellLyrClose(int chanId);
  static void cellLyrCloseNoVat(int chanId);
  static void getWindowBandFloat(
    int chanId, 
    int startrow, int nrRows,  float **ptrVal);
  static void getWindowBandInt(
    int chanId, 
    int startrow, int nrRows,  int **ptrVal);
  static void privateWindowBox(int chan_id,
                               double box[4]);
  static void privateAccessWindowSet(int chan_id,
  const double bndBox[4], double cellSize, double adjBndBox[4],
    size_t& nrRows, size_t& nrCols); 

  static void getMissingFloat(float *mvVal);

  static int  cellLayerCreate (
    const std::string& name, 
    int rdwrflag, 
    int iomode, 
                int celltype, double cellsize, const double box[4]);

  static void putWindowBand(
    int channel, int startrow, int nrows, const void * const * bandbuf);

  static int privateWindowCols(
    int chan_id);
  static int privateWindowRows(
    int chan_id);

  static void StaGetMinmaxDbl(
    const std::string& fileName, 
    double *min, double *max);
  static void putWindowCellInt(
    int channel, size_t col, size_t row, int icell);
  static void putWindowCellFloat(
    int channel, size_t col, size_t row, double fcell);
  static void gridDelete(const std::string& gridName);
  static void privateAccessWindowClear(int chanNr);
  static void cellLyrSta (int channel, double *dmin, double *dmax, 
                double *dmean, double *dstdv);
  static void cellLyrBlockSize(int channel, int *bxcells, int *bycells);
};

}

#endif
