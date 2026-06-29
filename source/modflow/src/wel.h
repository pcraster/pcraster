#ifndef INCLUDED_MODFLOW_WEL
#define INCLUDED_MODFLOW_WEL

#include "stddefx.h"
#include "discr_blockdata.h"

#include <string>


class PCRModflow;

namespace calc {
  class Field;
}


class WEL{
 protected:
 private:
  PCRModflow *d_mf;
  size_t           d_nr_wel_cells{0};

  int              d_output_unit_number{0};

  int              d_input_unit_number{280};
  
  std::string      d_filename{"pcrmf_wel.txt"};
  
  std::string      d_data_filename{"pcrmf_wel_data.txt"};

 public:
  WEL(PCRModflow *mf);
  ~WEL();
  //void writeWEL() const;
  bool setWell(const float *values, size_t mfLayer);
  void setWell(const discr::BlockData<REAL4> &well);
  void setWell(const calc::Field *well, size_t layer);


  calc::Field*     get_well            (size_t layer, std::string const& path);

  void             write               (std::string const& path);

  void             write_list          (std::string const& path);
  
  std::string      filename            () const {
    return d_filename;
  };
  
  std::string      data_filename       () const {
    return d_data_filename;
  };
};

#endif // INCLUDED_MODFLOW_WEL
