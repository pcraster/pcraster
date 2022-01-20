#ifndef INCLUDED_DSP
#define INCLUDED_DSP


// Library headers.
#include<ostream>

// PCRaster library headers.

// Module headers.



class DSP {

private:

  size_t           d_itmx{0};

  size_t           d_mxup{0};

  size_t           d_mxlow{0};

  size_t           d_mxbw{0};

  size_t           d_ifreq{0};

  size_t           d_mutd4{2};

  double           d_accl{-1.0};

  double           d_hclose{-1.0};

  size_t           d_ipdr4{999};

  bool             d_updated{true};

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
