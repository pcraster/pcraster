#ifndef INCLUDED_MODFLOW_GHB
#define INCLUDED_MODFLOW_GHB

#include "discr_block.h"
#include "discr_blockdata.h"
#include "calc_field.h"




class PCRModflow;


class GHB{
 protected:
 private:

  PCRModflow *d_mf;

  bool             d_ghbUpdated{false};

  size_t           d_nr_ghb_cells{0};

  int              d_output_unit_number{255};

  int              d_input_unit_number{256};

 public:

  GHB(PCRModflow *mf);

  ~GHB();

  bool             ghbUpdated          () const;

  void             setGhbUpdated       (bool value);

  void             setGHB              (const calc::Field * head, const calc::Field * cond, size_t layer);

  void             setGHB              (const float * head, const float *cond, size_t mfLayer);

  calc::Field*     getGhbLeakage       (size_t layer, std::string const & path) const;

  void             write               (std::string const& path);

  void             write_list          (std::string const& path);

};

#endif