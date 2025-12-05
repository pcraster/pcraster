#ifndef INCLUDED_MODFLOW_PCG
#define INCLUDED_MODFLOW_PCG

#include<ostream>


class PCG {

private:

  size_t           d_mxiter{0};

  size_t           d_iteri{0};

  size_t           d_npcond{0};

  double           d_hclose{-1.0};

  double           d_rclose{-1.0};

  double           d_relax{-1.0};

  double           d_nbpol{-1.0};

  double           d_iprpcg{1};

  size_t           d_mutpcg{3};

  double           d_damp{-1.0};

  bool             d_updated{true};

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


#endif // INCLUDED_MODFLOW_PCG

