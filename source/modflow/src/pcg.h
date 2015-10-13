#ifndef INCLUDED_PCG
#define INCLUDED_PCG


// Library headers.
#include<ostream>

// PCRaster library headers.

// Module headers.


class PCG {

private:

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

  bool             d_updated;

public:

                   ~PCG                ();

                   PCG                 ();

  void             setPCG              (size_t mxiter,
                                        size_t iteri,
                                        size_t npcond,
                                        double hclose,
                                        double rclose,
                                        double relax,
                                        double nbpol,
                                        double damp,
                                        bool updated);

  void             update              ();

  bool             modified            () const;

  friend std::ostream& operator<<      (std::ostream& os,
                                        const PCG& pcg);
};


#endif // INCLUDED_PCG

