#ifndef INCLUDED_SIP
#define INCLUDED_SIP

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.

// PCRaster library headers.

// Module headers.


class PCRModflow;

class SIP{
 protected:
 private:
  PCRModflow *d_mf;
  size_t d_mxiter;
  size_t d_nparam;
  double d_accl;
  double d_hclose;
  size_t d_ipcalc;
  double d_wseed;
  size_t d_iprsip;
 public:
  ~SIP();
  SIP(PCRModflow *mf, size_t mxiter, size_t nparam, double accl, double hclose, size_t ipcalc, double wseed);
  bool writeSIP() const;
};

#endif // INCLUDED_SIP
