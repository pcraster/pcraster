#ifndef INCLUDED_GRIDCHECK
#define INCLUDED_GRIDCHECK

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

// Module headers.



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


#endif // INCLUDED_GRIDCHECK
