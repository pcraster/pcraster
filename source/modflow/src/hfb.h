#ifndef INCLUDED_MODFLOW_HFB
#define INCLUDED_MODFLOW_HFB

#include "stddefx.h"


// Library headers.

// PCRaster library headers.

// Module headers.

class PCRModflow;

class HFB{
protected:
private:
  PCRModflow *d_mf;
public:
  ~HFB();
  HFB(PCRModflow *mf);
};

#endif // INCLUDED_MODFLOW_HFB
