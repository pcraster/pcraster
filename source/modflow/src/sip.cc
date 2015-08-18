#ifndef INCLUDED_SIP
#include "sip.h"
#define INCLUDED_SIP
#endif


// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
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
SIP::~SIP(){
}


/**
* Constructor
*/
SIP::SIP(PCRModflow *mf, size_t mxiter, size_t nparam, double accl, double hclose, size_t ipcalc, double wseed):
d_mf(mf),
d_mxiter(mxiter),
d_nparam(nparam),
d_accl(accl),
d_hclose(hclose),
d_ipcalc(ipcalc),
d_wseed(wseed),
d_iprsip(1){
}


/**
* writes SIP to file
*/
bool SIP::writeSIP() const{
  std::stringstream content;
  content << " " << std::setw(9) << d_mxiter;
  content << " " << std::setw(9) << d_nparam << std::endl;
  content << " " << std::setw(9) << d_accl;
  content << " " << std::setw(9) << d_hclose;
  content << " " << std::setw(9) << d_ipcalc;
  content << " " << std::setw(9) << d_wseed;
  content << " " << std::setw(9) << d_iprsip << std::endl;

  return d_mf->d_cmethods->writeToFile("pcrmf.sip",content.str());
}

