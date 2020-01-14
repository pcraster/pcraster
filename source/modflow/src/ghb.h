#pragma once

// Library headers.

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




class PCRModflow;


class GHB{
 protected:
 private:

  PCRModflow *d_mf;

  bool d_ghbUpdated;

  size_t           d_nr_ghb_cells;

  int              d_output_unit_number;

  int              d_input_unit_number;

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
