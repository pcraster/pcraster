#ifndef INCLUDED_BAS
#define INCLUDED_BAS

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif


// PCRaster library headers.
#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif


// Module headers.
//#ifndef INCLUDED_PCRMODFLOW
//#include "pcrmodflow.h"
//#define INCLUDED_PCRMODFLOW
//#endif


class PCRModflow;

class BAS{
protected:
private:
  PCRModflow *d_mf;
  double d_hnoflo;
  int              d_fortran_unit_number_heads;

  int              d_fortran_unit_number_bounds;


  int              d_external_unit_number_heads;

  int              d_external_unit_number_bounds;

public:
  ~BAS();
  BAS(PCRModflow *mf);
  void writeBAS();
  void setNoFlowConstant(float value);
  void setBASBlockData(const discr::BlockData<INT4> &source, discr::BlockData<INT4> &result);
  void setBASBlockData(const discr::BlockData<REAL4> &source, discr::BlockData<REAL4> &result);

  void getBASBlockData(discr::BlockData<INT4> &bdata, std::string const& path);
  //void getBASBlockData(const std::string &filename, discr::BlockData<REAL4> &bdata);
  void getHeads(float *result, size_t mfLayer);
  void getHeadsFromBinary(std::string const& path);
  calc::Field* getHeads(size_t mfLayer);
  discr::BlockData<REAL4>* getHeads();
  void setIBound(const calc::Field *values, size_t layer);
  void setInitialHead(const calc::Field *values, size_t layer);

  int              fortran_unit_number_heads() const;

  int              fortran_unit_number_bounds() const;

  void             write               (std::string const& path) const;

  void             write_head_array    (std::string const& path) const;

  void             write_bound_array   (std::string const& path) const;
};

#endif // INCLUDED_BAS
