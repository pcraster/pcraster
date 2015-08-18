#ifndef INCLUDED_PCG
#define INCLUDED_PCG

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.

// PCRaster library headers.

// Module headers.


class PCRModflow;

class PCG {

private:

  PCRModflow       *d_mf;

  size_t           d_mxiter;

  size_t           d_iteri;

  size_t           d_npcond;

  double           d_hclose;

  double           d_rclose;

  double           d_relax;

  double           d_nbpol;

  double           d_iprpcg;

  size_t           d_mutpcg;

  double           d_damp;

public:

                   ~PCG                ();

                   PCG                 (PCRModflow *mf,
                                        size_t mxiter,
                                        size_t iteri,
                                        size_t npcond,
                                        double hclose,
                                        double rclose,
                                        double relax,
                                        double nbpol,
                                        double damp);

  bool             writePCG            () const;
};


#endif // INCLUDED_PCG

