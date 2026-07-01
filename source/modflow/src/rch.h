#ifndef INCLUDED_MODFLOW_RCH
#define INCLUDED_MODFLOW_RCH

#include "stddefx.h"
#include "calc_field.h"
#include "discr_block.h"

#include <string>


class PCRModflow;

class RCH{
 protected:
 private:
  PCRModflow *d_mf;
  size_t d_nrchop;
  //short d_irchcb;
  short d_inrech{1};
  short d_inirch{1};
  //int              d_fortran_unit_number;

  int              d_output_unit_number{260};

  int              d_array_unit_number{261};

  int              d_indicated_unit_number{262};

  std::string      d_filename{"pcrmf_rch.txt"};
  
  std::string      d_data_rch_filename{"pcrmf_rch_data.txt"};
  
  std::string      d_data_irch_filename{"pcrmf_irch_data.txt"};
  
  std::string      d_output_rch_filename{"pcrmf_rch.bin"};
  
 public:
  ~RCH();
  RCH(PCRModflow *mf, size_t option);
  void setRecharge(const calc::Field *rch, size_t optCode);
  void setIndicatedRecharge(const calc::Field *rch, const calc::Field *layer);
  //void writeRCH() const;

  //void getFlowFromBinary();
//  discr::BlockData<REAL4>* getBlockCellByCellFlow();

  void             getRecharge         (float * values, size_t mfLayer, std::string const& path) const;

  calc::Field*     getRecharge         (size_t layer, std::string const& path) const;

  void             write               (std::string const& path);

  void             write_array         (std::string const& path);

  void             write_indicated     (std::string const& path);

  bool             indicated_recharge  () const;
  
  std::string      filename            () const {
    return d_filename;
  };
  
  std::string      data_rch_filename   () const {
    return d_data_rch_filename;
  };
  
  std::string      data_irch_filename  () const {
    return d_data_irch_filename;
  };
  
  std::string      output_rch_filename  () const {
    return d_output_rch_filename;
  };
};

#endif // INCLUDED_MODFLOW_RCH
