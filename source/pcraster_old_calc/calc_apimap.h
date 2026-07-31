#ifndef INCLUDED_OLDCALC_APIMAP
#define INCLUDED_OLDCALC_APIMAP

#include "csftypes.h"
#include "calc_objcount.h"
#include "api.h"
#include "geo_rasterspace.h"



namespace calc {

class ApiMap: private ObjCount<ApiMap> {
 protected:
   static  void* allocate(CSF_CR inCr, size_t nrCells);
 public:
   virtual      ~ApiMap() {}

   virtual void *getCPointer()=0;
   virtual void *detachData()=0;
};

template<typename MAP_API>
class ApiMapC : public ApiMap {
  using InitMap = MAP_API *(*)(size_t, size_t, void *, bool, CSF_CR);
  using DeleteInternal = void (*)(MAP_API *);
  static   InitMap d_init;
  static   DeleteInternal d_del;

  // hack keep it to return for result (ResultArg only)
  void    *d_data;
  MAP_API *d_map;

 public:
    //! ctor for read-only, GlobArg
    ApiMapC(const geo::RasterSpace& rs,const void *val,bool spatial,CSF_CR inCr):
      d_data(nullptr),
      d_map(d_init(rs.nrRows(),rs.nrCols(),const_cast<void *>(val),static_cast<int>(spatial),inCr)) {}
    //! ctor, always spatial, allocate data area
    ApiMapC(const geo::RasterSpace& rs,CSF_CR inCr):
      d_data(allocate(inCr,rs.nrCells())),
      d_map(d_init(rs.nrRows(),rs.nrCols(),d_data,true,inCr)) {}
    ~ApiMapC() override {
      d_del(d_map);
      d_map=nullptr;
      delete [] ((unsigned char *)d_data);
      d_data=nullptr;
    }
   void *getCPointer() override {
    return (void *)d_map;
   }
   void *detachData() override {
     PRECOND(d_data);
     void *v=d_data;
     d_data=nullptr;
     return v;
   }
};

 using ApiMapUINT1 = ApiMapC<MAP_UINT1>;
 using ApiMapINT4 = ApiMapC<MAP_INT4>;
 using ApiMapREAL8 = ApiMapC<MAP_REAL8>;

}

#endif
