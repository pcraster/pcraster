#ifndef INCLUDED_MODFLOW_SOR
#define INCLUDED_MODFLOW_SOR

#include <ostream>



class SOR {

private:

  size_t           d_mxiter{0};

  double           d_accl{-1.0};

  double           d_hclose{-1.0};

  size_t           d_iprsor{0};

  bool             d_updated{true};

public:

                   ~SOR();

                   SOR();

  void             setSOR              (size_t mxiter,
                                        double accl,
                                        double hclose,
                                        bool updated);

  void             update              ();

  bool             modified            () const;

  friend std::ostream& operator<<      (std::ostream& os,
                                        const SOR& sor);
};

#endif // INCLUDED_MODFLOW_SOR
