#ifndef INCLUDED_DSP
#define INCLUDED_DSP

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.

// PCRaster library headers.

// Module headers.


class PCRModflow;

class DSP{
protected:
private:
  PCRModflow *d_mf;
  size_t d_itmx;
  size_t d_mxup;
  size_t d_mxlow;
  size_t d_mxbw;
  size_t d_ifreq;
  size_t d_mutd4;
  double d_accl;
  double d_hclose;
  size_t d_ipdr4;

public:
  ~DSP();
  DSP(PCRModflow *mf, size_t itmx, size_t mxup, size_t mxlow, size_t mxbw, size_t ifreq, double accl, double hclose);
  bool writeDSP() const;
};

#endif // INCLUDED_DSP
