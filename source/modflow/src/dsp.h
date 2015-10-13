#ifndef INCLUDED_DSP
#define INCLUDED_DSP


// Library headers.
#include<ostream>

// PCRaster library headers.

// Module headers.



class DSP {

private:

  size_t           d_itmx;

  size_t           d_mxup;

  size_t           d_mxlow;

  size_t           d_mxbw;

  size_t           d_ifreq;

  size_t           d_mutd4;

  double           d_accl;

  double           d_hclose;

  size_t           d_ipdr4;

  bool             d_updated;

public:

                   ~DSP();

                   DSP();

  void             setDSP              (size_t itmx,
                                        size_t mxup,
                                        size_t mxlow,
                                        size_t mxbw,
                                        size_t ifreq,
                                        double accl,
                                        double hclose,
                                        bool updated);

  void             update              ();

  bool             modified            () const;

  friend std::ostream& operator<<      (std::ostream& os,
                                        const DSP& dsp);
};

#endif // INCLUDED_DSP
