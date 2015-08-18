#ifndef INCLUDED_CALC_ESRIMAP
#define INCLUDED_CALC_ESRIMAP

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_CALC_GRIDMAP
#include "calc_gridmap.h"
#define INCLUDED_CALC_GRIDMAP
#endif

namespace calc {

//! exception class for reading/creating calc::EsriMap
struct NotAnEsriGrid : public com::Exception {
 NotAnEsriGrid();
};

//! esri grid
class EsriMap : public GridMap {
private:
  //! illegal
  EsriMap();
  //! illegal
  EsriMap(const EsriMap&);

  enum GRIDSIZE { ROW_GRIDSIZE=0, COL_GRIDSIZE=1};
  double d_cellSize;
  // index:
  // 0 : left
  // 1 : bottom
  // 2 : right
  // 3 : top
  // 4 : paranoia
  double d_box[5];

  void close() const;
  void readFloat(void *val);
  void readInt(void *val);

  bool cmpBox(const double *otherBox) const;


  //! mutable is for minMax hack
  mutable int d_chanId;

  //! empty if none set, for exising map it is searched for
  std::string d_prjFile;

  //! class for windowing
  class Window {
   private:
    int d_winChan;
    //! illegal
    Window();
   public:
    Window(int winChan);
    ~Window();
  };

  Window window();

public:
  // CREATORS

  EsriMap(const std::string& fileName);

  EsriMap( const std::string& fileName,
    size_t nrRows,
    size_t nrCols,
    double cellSize,
    const double box[4], VS vs);

  //! close map
    ~EsriMap();

    static bool remove(const std::string& gridName);
    static bool exists(const std::string& gridName);

  // ACCESSORS

  double cellSize() const
  {
    return d_cellSize;
  }

  void        setPrjFile(const std::string prjFile);
  std::string prjFile   () const;
  com::PathName prjFilePath() const;

  void bbox(double *setThis) const;

  bool getMinMax(double& min, double& max) const;

  void readInBuffer(VS readAs, void *val);

  void writeData(const void *allValues);
};

}

#endif
