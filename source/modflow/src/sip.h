#ifndef INCLUDED_SIP
#define INCLUDED_SIP


// Library headers.
#include<ostream>

// PCRaster library headers.

// Module headers.


class SIP {

private:
  size_t           d_mxiter;

  size_t           d_nparam;

  double           d_accl;

  double           d_hclose;

  size_t           d_ipcalc;

  double           d_wseed;

  size_t           d_iprsip;

  bool             d_updated;

 public:

                   ~SIP();

                   SIP();

  void             setSIP              (size_t mxiter,
                                        size_t nparam,
                                        double accl,
                                        double hclose,
                                        size_t ipcalc,
                                        double wseed,
                                        bool updated);

  void             update              ();

  bool             modified            () const;

  friend std::ostream& operator<<      (std::ostream& os,
                                        const SIP& sip);
};

#endif // INCLUDED_SIP
