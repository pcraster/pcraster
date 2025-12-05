#ifndef INCLUDED_MODFLOW_GRIDCHECK
#define INCLUDED_MODFLOW_GRIDCHECK

#include "stddefx.h"

#include <string>



class PCRModflow;

class GridCheck{
 protected:
 private:
  PCRModflow *d_mf;
 public:
  ~GridCheck();
  GridCheck(PCRModflow *mf);
  void isGrid(size_t mfLayer, const std::string &methodName);
  void setVCond(size_t mfLayer, const std::string &methodName);
  void testMV(const float *values, const std::string &methodName);
  void testMV(const int *values, const std::string &methodName);
  void testElevation();
  void isConfined(size_t layer, const std::string &methodName);
};


#endif // INCLUDED_MODFLOW_GRIDCHECK
