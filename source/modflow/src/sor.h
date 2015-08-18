#ifndef INCLUDED_SOR
#define INCLUDED_SOR

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.

// PCRaster library headers.

// Module headers.



class PCRModflow;

class SOR{
 protected:
 private:
  PCRModflow *d_mf;
  size_t d_mxiter;
  double d_accl;
  double d_hclose;
  size_t d_iprsor;
 public:
  ~SOR();
  SOR(PCRModflow *mf, size_t mxiter, double accl, double hclose);
  bool writeSOR() const;
};

#endif // INCLUDED_SOR
