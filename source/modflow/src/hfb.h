#ifndef INCLUDED_HFB
#define INCLUDED_HFB

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


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

#endif // INCLUDED_HFB
