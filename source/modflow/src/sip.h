#ifndef INCLUDED_SIP
#define INCLUDED_SIP


// Library headers.
#include<ostream>

// PCRaster library headers.

// Module headers.


class SIP {

private:
  size_t           d_mxiter{0};

  size_t           d_nparam{0};

  double           d_accl{-1.0};

  double           d_hclose{-1.0};

  size_t           d_ipcalc{0};

  double           d_wseed{-1.0};

  size_t           d_iprsip{1};

  bool             d_updated{true};

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
