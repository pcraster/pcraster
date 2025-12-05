#ifndef INCLUDED_MODFLOW_BAS
#define INCLUDED_MODFLOW_BAS

#include "stddefx.h"
#include "discr_block.h"
#include "discr_blockdata.h"
#include "calc_field.h"

#include <string>



class PCRModflow;

class BAS{
protected:
private:
  PCRModflow *d_mf;
  double d_hnoflo{-999.9};
  int              d_fortran_unit_number_heads{231};

  int              d_fortran_unit_number_bounds{232};


  int              d_external_unit_number_heads{400};

  int              d_external_unit_number_bounds{401};

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

#endif // INCLUDED_MODFLOW_BAS
