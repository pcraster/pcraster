#ifndef INCLUDED_DIS
#define INCLUDED_DIS

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
#include <discr_block.h>
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_BLOCKDATA
#include <discr_blockdata.h>
#define INCLUDED_DISCR_BLOCKDATA
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

// Module headers.

class PCRModflow;

class DIS{
protected:
private:
  PCRModflow *d_mf;
  size_t d_itmuni;
  size_t d_lenuni;
  float d_perlen;
  size_t d_nstp;
  float d_tsmult;
  std::string d_sstr;
  bool addLayer(const float *values, bool confined);
public:
  ~DIS();
  DIS(PCRModflow *mf);
  bool createBottom(const float *lower, const float *upper);
  bool addLayer(const float *values);
  bool addConfinedLayer(const float *values);
  void setLayer(const discr::Block &elevation, const discr::BlockData<INT4> &conf);
  void createBottom(const calc::Field *lower, const calc::Field *upper);
  void addLayer(const calc::Field *values);
  void addConfinedLayer(const calc::Field *values);
  bool writeDIS() const;
  void setParams(size_t itmuni, size_t lenuni, float perlen, size_t nstp, float tsmult, bool sstr);
  size_t getTimeSteps() const;
};

#endif // INCLUDED_DIS
