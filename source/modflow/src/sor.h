#ifndef INCLUDED_SOR
#define INCLUDED_SOR


// Library headers.
#include<ostream>

// PCRaster library headers.

// Module headers.



class SOR {

private:

  size_t           d_mxiter;

  double           d_accl;

  double           d_hclose;

  size_t           d_iprsor;

  bool             d_updated;

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

#endif // INCLUDED_SOR
