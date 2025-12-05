#ifndef INCLUDED_MODFLOW_RIV
#define INCLUDED_MODFLOW_RIV

#include "stddefx.h"
#include "discr_block.h"
#include "discr_blockdata.h"
#include "calc_field.h"




class PCRModflow;


class RIV{
 protected:
 private:
  PCRModflow *d_mf;
  bool d_riverUpdated{false};

  size_t           d_nr_river_cells{0};

  int              d_output_unit_number{250};

  int              d_input_unit_number{251};

  //int              d_fortran_unit_number;

 public:
  ~RIV();
  RIV(PCRModflow *mf);
  //bool writeRIV() const;
  bool riverUpdated() const;
  void setRiverUpdated(bool value);
  bool setRiver(const float *rivH, const float *rivB, const float *rivC, size_t layer);
  void setRiver(const calc::Field *rivH, const calc::Field *rivB, const calc::Field *rivC, size_t layer);
  void setRiver(const discr::BlockData<REAL4> &stage, const discr::BlockData<REAL4> &bottom, const discr::BlockData<REAL4> &cond);
  //discr::BlockData<REAL4>* getBlockCellByCellFlow();

  calc::Field* getRiverLeakage(size_t layer, std::string const& path) const;
  void getRiverLeakage(float *result, size_t layer, std::string const& path) const;

  //void getRivLeakFromBinary();

  void             write               (std::string const& path);

  void             write_list          (std::string const& path);

};

#endif // INCLUDED_MODFLOW_RIV
