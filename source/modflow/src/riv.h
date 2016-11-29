#ifndef INCLUDED_RIV
#define INCLUDED_RIV

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


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


class RIV{
 protected:
 private:
  PCRModflow *d_mf;
  bool d_riverUpdated;

  size_t           d_nr_river_cells;

  int              d_output_unit_number;

  int              d_input_unit_number;

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

#endif // INCLUDED_RIV
