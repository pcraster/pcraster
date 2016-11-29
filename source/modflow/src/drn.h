#ifndef INCLUDED_DRN
#define INCLUDED_DRN

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


class PCRModflow;

class DRN{
protected:
private:
  PCRModflow *d_mf;
  bool d_drainUpdated;

  size_t           d_nr_drain_cells;

  int              d_output_unit_number;

  int              d_input_unit_number;


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
};

#endif // INCLUDED_DRN
