#ifndef INCLUDED_MODFLOW_DRN
#define INCLUDED_MODFLOW_DRN

#include "stddefx.h"
#include "discr_block.h"
#include "discr_blockdata.h"
#include "calc_field.h"

#include <string>


class PCRModflow;

class DRN{
protected:
private:
  PCRModflow *d_mf;
  bool d_drainUpdated{false};

  size_t           d_nr_drain_cells{0};

  int              d_output_unit_number{271};

  int              d_input_unit_number{270};
  
  std::string      d_filename{"pcrmf_drn.txt"};
  
  std::string      d_data_filename{"pcrmf_drn_data.txt"};
  
  std::string      d_output_drn_filename{"pcrmf_drn.bin"};

public:
  DRN(PCRModflow *mf);
  ~DRN();
  //bool writeDRN() const;
  bool drainUpdated() const;
  void setDrainUpdated(bool value);
  bool setDrain(const float *elevation, const float *conductance, size_t mfLayer);
  void setDrain(const discr::BlockData<REAL4> &elevation, const discr::BlockData<REAL4> &conductance);
  void setDrain(const calc::Field *elevation, const calc::Field *conductance, size_t layer);
  //void getDrainFromBinary();
  //discr::BlockData<REAL4>* getBlockCellByCellFlow();


  void getDrain(float *values, size_t mfLayer, std::string const& path) const;

  calc::Field* getDrain(size_t layer, std::string const& path) const;

  void             write               (std::string const& path) const;

  void             write_list          (std::string const& path);
  
  std::string      filename            () const {
    return d_filename;
  };
  
  std::string      data_filename       () const {
    return d_data_filename;
  };
  
  std::string      output_drain_filename  () const {
    return d_output_drn_filename;
  };
};

#endif // INCLUDED_DMODFLOW_RN
