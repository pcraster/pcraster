#ifndef INCLUDED_DSP
#include "dsp.h"
#define INCLUDED_DSP
#endif


// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_IOMANIP
#include <iomanip>
#define INCLUDED_IOMANIP
#endif


// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COMMON
#include "common.h"
#define INCLUDED_COMMON
#endif

#ifndef INCLUDED_PCRMODFLOW
#include "pcrmodflow.h"
#define INCLUDED_PCRMODFLOW
#endif

/**
* Destructor
*/
DSP::~DSP(){
}


/**
* Constructor
*/
DSP::DSP(PCRModflow *mf, size_t itmx, size_t mxup, size_t mxlow, size_t mxbw, size_t ifreq, double accl, double hclose) :
d_mf(mf),
d_itmx(itmx),
d_mxup(mxup),
d_mxlow(mxlow),
d_mxbw(mxbw),
d_ifreq(ifreq),
d_mutd4(2),
d_accl(accl),
d_hclose(hclose),
d_ipdr4(999){

}


/**
* writeDSP
* writes Direct Solver Package to file
*/
bool DSP::writeDSP() const{
  std::stringstream content;
  content << " " << std::setw(9) << d_itmx;
  content << " " << std::setw(9) << d_mxup;
  content << " " << std::setw(9) << d_mxlow;
  content << " " << std::setw(9) << d_mxbw << std::endl;
  content << " " << std::setw(9) << d_ifreq;
  content << " " << std::setw(9) << d_mutd4;
  content << " " << std::setw(9) << d_accl;
  content << " " << std::setw(9) << d_hclose;
  content << " " << std::setw(9) << d_ipdr4 << std::endl;
  return d_mf->d_cmethods->writeToFile("pcrmf.de4",content.str());
}

