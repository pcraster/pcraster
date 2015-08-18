#ifndef INCLUDED_SOR
#include "sor.h"
#define INCLUDED_SOR
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
SOR::~SOR(){
}


/**
* Constructor
*/
SOR::SOR(PCRModflow *mf, size_t mxiter, double accl, double hclose) :
d_mf(mf),
d_mxiter(mxiter),
d_accl(accl),
d_hclose(hclose),
d_iprsor(0){
}



/**
* writes SOR to file
*/
bool SOR::writeSOR() const{
  std::stringstream content;
  content << " " << std::setw(9) << d_mxiter << std::endl;
  content << " " << std::setw(9) << d_accl;
  content << " " << std::setw(9) << d_hclose;
  content << " " << std::setw(9) << d_iprsor << std::endl;

  return d_mf->d_cmethods->writeToFile("pcrmf.sor",content.str());
}

