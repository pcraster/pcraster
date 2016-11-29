#ifndef INCLUDED_RCH
#define INCLUDED_RCH

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

// #ifndef INCLUDED_DISCR_BLOCKDATA
// #include "discr_blockdata.h"
// #define INCLUDED_DISCR_BLOCKDATA
// #endif


// Module headers.


class PCRModflow;

class RCH{
 protected:
 private:
  PCRModflow *d_mf;
  size_t d_nrchop;
  //short d_irchcb;
  short d_inrech;
  short d_inirch;
  //int              d_fortran_unit_number;

  int              d_output_unit_number;

  int              d_array_unit_number;

  int              d_indicated_unit_number;

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
};

#endif // INCLUDED_RCH
