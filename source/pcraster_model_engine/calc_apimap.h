#ifndef INCLUDED_CALC_APIMAP
#define INCLUDED_CALC_APIMAP

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

#ifndef INCLUDED_API
#include "api.h"
#define INCLUDED_API
#endif

#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif

namespace geo {
}

namespace calc {
//! interface of ApiMapC objects
class ApiMap {
 public:
   virtual      ~ApiMap() {};

   virtual void *getCPointer()=0;
};

//! wrappers around data for the libcalc function with a MAP_* interface
/*!
 * this class only wraps data of Spatial or NonSpatial it does not allocate;
 * the \a val arguments of both ctors are typically Field::src() or Field::dest().
 */
template<typename MAP_API>
class ApiMapC : public ApiMap {
  typedef  MAP_API *(* InitMap)   (size_t nrRows,size_t nrCols,
                                   void *v,BOOL spatial, CSF_CR inCr);
  typedef  void (* DeleteInternal)(MAP_API *m);

  static   InitMap        d_init;
  static   DeleteInternal d_del;

  MAP_API                *d_map;

 public:
    //! ctor for read-only, GlobArg
    ApiMapC(const geo::RasterDim& rs,const void *val,bool spatial,CSF_CR inCr):
      d_map(d_init(rs.nrRows(),rs.nrCols(),(void *)val,(int)spatial,inCr)) {};

    //! ctor for result, always spatial; GlobResult
    ApiMapC(const geo::RasterDim& rs, void *val, CSF_CR inCr):
      d_map(d_init(rs.nrRows(),rs.nrCols(),val,true,inCr)) {};
    ~ApiMapC() {
      d_del(d_map);
      d_map=0;
    }
   void *getCPointer() {
    return (void *)d_map;
   }
   MAP_API *map() const {
     return d_map;
   }
};

 typedef ApiMapC<MAP_UINT1> ApiMapUINT1;
 typedef ApiMapC<MAP_INT4>  ApiMapINT4;
 typedef ApiMapC<MAP_REAL8> ApiMapREAL8;

}

#endif
