#ifndef INCLUDED_CALC_APIMAP
#define INCLUDED_CALC_APIMAP

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

#ifndef INCLUDED_CALC_OBJCOUNT
#include "calc_objcount.h"
#define INCLUDED_CALC_OBJCOUNT
#endif

#ifndef INCLUDED_API
#include "api.h"
#define INCLUDED_API
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

namespace calc {

class ApiMap: private ObjCount<ApiMap> {
 protected:
   static  void* allocate(CSF_CR inCr, size_t nrCells);
 public:
   virtual      ~ApiMap() {};

   virtual void *getCPointer()=0;
   virtual void *detachData()=0;
};

template<typename MAP_API>
class ApiMapC : public ApiMap {
  typedef  MAP_API *(* InitMap)(size_t nrRows,size_t nrCols,void *v,BOOL spatial, CSF_CR inCr);
  typedef  void (* DeleteInternal)(MAP_API *m);
  static   InitMap d_init;
  static   DeleteInternal d_del;

  // hack keep it to return for result (ResultArg only)
  void    *d_data;
  MAP_API *d_map;

 public:
    //! ctor for read-only, GlobArg
    ApiMapC(const geo::RasterSpace& rs,const void *val,bool spatial,CSF_CR inCr):
      d_data(0),
      d_map(d_init(rs.nrRows(),rs.nrCols(),(void *)val,(int)spatial,inCr)) {};
    //! ctor, always spatial, allocate data area
    ApiMapC(const geo::RasterSpace& rs,CSF_CR inCr):
      d_data(allocate(inCr,rs.nrCells())),
      d_map(d_init(rs.nrRows(),rs.nrCols(),d_data,true,inCr)) {};
    ~ApiMapC() {
      d_del(d_map);
      d_map=0;
      delete [] ((unsigned char *)d_data);
      d_data=0;
    }
   void *getCPointer() {
    return (void *)d_map;
   }
   void *detachData() {
     PRECOND(d_data);
     void *v=d_data;
     d_data=0;
     return v;
   }
};

 typedef ApiMapC<MAP_UINT1> ApiMapUINT1;
 typedef ApiMapC<MAP_INT4>  ApiMapINT4;
 typedef ApiMapC<MAP_REAL8> ApiMapREAL8;

}

#endif
