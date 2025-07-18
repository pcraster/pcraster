#include "sor.h"

/**
* Destructor
*/
SOR::~SOR(){
}


/**
* Constructor
*/
SOR::SOR() 
  {
}


void SOR::setSOR(size_t mxiter, double accl, double hclose, bool updated){
  d_mxiter = mxiter;
  d_accl = accl;
  d_hclose = hclose;
  d_iprsor = 0;
  d_updated = updated;
}


bool SOR::modified() const {
  return d_updated;
}


void SOR::update() {
  d_updated = false;
}


std::ostream& operator<<(std::ostream& os, const SOR& sor){
  os << "# Generated by PCRaster Modflow\n";
  os << sor.d_mxiter << "\n";
  os << sor.d_accl << " " << sor.d_hclose << " " << sor.d_iprsor << "\n";
  return os;
}

