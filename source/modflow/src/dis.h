#ifndef INCLUDED_DIS
#define INCLUDED_DIS

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.
#ifndef INCLUDED_STRING
#include <sstream>
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
  float            d_tsmult;
  std::string d_sstr;

  std::vector<float> d_row_width;

  std::vector<float> d_col_width;

  size_t           d_external_unit;

  bool addLayer(const float *values, bool confined);

  void             write_row_width     (std::ostringstream& content) const;

  void             write_col_width     (std::ostringstream& content) const;


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
  //bool writeDIS() const;
  void setParams(size_t itmuni, size_t lenuni, float perlen, size_t nstp, float tsmult, bool sstr);
  size_t getTimeSteps() const;


  void             reset_row_width     ();

  void             append_row_width    (float width);

  void             reset_col_width     ();

  void             append_col_width    (float width);

  void             write_dis           (std::string const& path) const;

  void             write_dis_array     (std::string const& path) const;

  void             update_parameter    (float stressPeriodLength, size_t nrOfTimesteps, float timeStepMultiplier);
};

#endif // INCLUDED_DIS
