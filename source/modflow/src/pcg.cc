#ifndef INCLUDED_PCG
#include "pcg.h"
#define INCLUDED_PCG
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
PCG::~PCG(){
}

/**
* Constructor
*/
PCG::PCG(PCRModflow *mf, size_t mxiter, size_t iteri, size_t npcond, double hclose, double rclose, double relax, double nbpol, double damp) :
d_mf(mf),
d_mxiter(mxiter),
d_iteri(iteri),
d_npcond(npcond),
d_hclose(hclose),
d_rclose(rclose),
d_relax(relax),
d_nbpol(nbpol),
d_iprpcg(1),
d_mutpcg(3),  // append to lst only if convergence fails
d_damp(damp){
}

/**
* write PCG to file
*/
bool PCG::writePCG() const{
  std::stringstream content;
  content << " " << std::setw(9) << d_mxiter;
  content << " " << std::setw(9) << d_iteri;
  content << " " << std::setw(9) << d_npcond << std::endl;
  content << " " << std::setw(9) << d_hclose;
  content << " " << std::setw(9) << d_rclose;
  content << " " << std::setw(9) << d_relax;
  content << " " << std::setw(9) << d_nbpol;
  content << " " << std::setw(9) << d_iprpcg;
  content << " " << std::setw(9) << d_mutpcg;
  content << " " << std::setw(9) << d_damp << std::endl;

  return d_mf->d_cmethods->writeToFile("pcrmf.pcg",content.str());
}
