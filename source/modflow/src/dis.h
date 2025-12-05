#ifndef INCLUDED_MODFLOW_DIS
#define INCLUDED_MODFLOW_DIS

#include "stddefx.h"
#include "discr_block.h"
#include "discr_blockdata.h"
#include "calc_field.h"

#include <sstream>



class PCRModflow;

class DIS{
protected:
private:
  PCRModflow *d_mf;
  size_t d_itmuni{0};
  size_t d_lenuni{0};
  float d_perlen{1.0};
  size_t d_nstp{1};
  float            d_tsmult{1.0};
  std::string d_sstr;

  std::vector<float> d_row_width;

  std::vector<float> d_col_width;

  size_t           d_external_unit{300};

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

#endif // INCLUDED_MODFLOW_DIS
