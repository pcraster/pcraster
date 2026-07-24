#include "pcr_mflink.h"


ModflowLink::ModflowLink(pcrxml::LinkInExecuteInput const& l){
  size_t const rows = l.context().areaMap().nrRows();
  size_t const cols = l.context().areaMap().nrCols();
  double const cells = l.context().areaMap().cellSize().get();
  double const west = l.context().areaMap().xLowerLeftCorner().get();
  double const north = l.context().areaMap().yLowerLeftCorner().get();

  d_pcrmf = new PCRModflow(rows, cols, cells, west, north);
}

ModflowLink::~ModflowLink(){
  delete d_pcrmf;
}

void ModflowLink::run(){
  d_pcrmf->runModflow("");
}

/**
 * creating the bottom layer
 */
void  ModflowLink::createBottom(LinkInTransferArray linkInTransferArray){
  auto *lower = static_cast<float *>(linkInTransferArray[1]);
  auto *upper = static_cast<float *>(linkInTransferArray[2]);
  d_pcrmf->createBottom(lower, upper);

}

/**
 * adding a layer on top of the grid
 */
void  ModflowLink::addLayer(LinkInTransferArray linkInTransferArray){
  auto *values = static_cast<float *>(linkInTransferArray[1]);
  d_pcrmf->addLayer(values);
}

/**
 * adding a confined layer on top of the grid
 */
void  ModflowLink::addConfinedLayer(LinkInTransferArray linkInTransferArray){
  auto *values = static_cast<float *>(linkInTransferArray[1]);
  d_pcrmf->addConfinedLayer(values);
}


void ModflowLink::setBoundary(LinkInTransferArray linkInTransferArray){
  int *values = static_cast<int *>(linkInTransferArray[1]);
  int const layer = (static_cast<const int *>(linkInTransferArray[2]))[0];
  d_pcrmf->setIBound(values, layer);
}


void ModflowLink::setHead(LinkInTransferArray linkInTransferArray){
  auto *values    =static_cast<float *>(linkInTransferArray[1]);
  int const layer    = (static_cast<const int *>(linkInTransferArray[2]))[0];
  d_pcrmf->setInitialHead(values, layer);
}


void ModflowLink::setConductivity(LinkInTransferArray linkInTransferArray){
  int const laycon    = (static_cast<const int *>(linkInTransferArray[1]))[0];
  auto *hConds    =static_cast<float *>(linkInTransferArray[2]);
  auto *vConds    =static_cast<float *>(linkInTransferArray[3]);
  int const layer    = (static_cast<const int *>(linkInTransferArray[4]))[0];

  d_pcrmf->setHCond(hConds, layer, laycon);
  d_pcrmf->setVCond(vConds, layer);
}



void ModflowLink::getHead(LinkInTransferArray linkInTransferArray){
  auto *result = static_cast<float *>(linkInTransferArray[0]);
  int const layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->getHeads(result, layer);
}

void ModflowLink::getRivLeak(LinkInTransferArray linkInTransferArray){
  auto *result = static_cast<float *>(linkInTransferArray[0]);
  int const layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->getRiverLeakage(result, layer);
}



void ModflowLink::setRiver(LinkInTransferArray linkInTransferArray){
  auto *rivH    =static_cast<float *>(linkInTransferArray[1]);
  auto *rivB    =static_cast<float *>(linkInTransferArray[2]);
  auto *rivC    =static_cast<float *>(linkInTransferArray[3]);
  int const mfLayer    = (static_cast<const int *>(linkInTransferArray[4]))[0];
  d_pcrmf->setRiver(rivH, rivB, rivC, mfLayer);
}


void ModflowLink::setDISParams(LinkInTransferArray linkInTransferArray){
  int const time = (static_cast<const int *>(linkInTransferArray[1]))[0];
  int const len = (static_cast<const int *>(linkInTransferArray[2]))[0];
  float const stresslen = (static_cast<const float *>(linkInTransferArray[3]))[0];
  int const tSteps = (static_cast<const int *>(linkInTransferArray[4]))[0];
  float const tMult = (static_cast<const float *>(linkInTransferArray[5]))[0];
  bool const ss = (static_cast<const bool *>(linkInTransferArray[6]))[0];
  d_pcrmf->setDISParams(time,len,stresslen,tSteps,tMult,ss);
}



void ModflowLink::setHNOFLO(LinkInTransferArray linkInTransferArray){
  const float val = (static_cast<const float *>(linkInTransferArray[1]))[0];
  d_pcrmf->setNoFlowConstant(val);
}



void ModflowLink::setTRPY(LinkInTransferArray linkInTransferArray){
  float const value = (static_cast<const float *>(linkInTransferArray[1]))[0];
  d_pcrmf->setTRPY(value);
}

void ModflowLink::setHDRY(LinkInTransferArray linkInTransferArray){
  float const value = (static_cast<const float *>(linkInTransferArray[1]))[0];
  d_pcrmf->setHDRY(value);
}

void ModflowLink::setWettingParameter(LinkInTransferArray linkInTransferArray){
  float const wetfct = (static_cast<const float *>(linkInTransferArray[1]))[0];
  int const iwetit = (static_cast<const int *>(linkInTransferArray[2]))[0];
  float const ihdwet = (static_cast<const float *>(linkInTransferArray[3]))[0];
  d_pcrmf->setWettingParameter(wetfct, iwetit, ihdwet);
}

void ModflowLink::setStorage(LinkInTransferArray linkInTransferArray){
  auto *primaryValues = static_cast<float *>(linkInTransferArray[1]);
  auto *secondaryValues = static_cast<float *>(linkInTransferArray[2]);
  int const mfLayer = (static_cast<const int *>(linkInTransferArray[3]))[0];
  d_pcrmf->setPrimaryStorage(primaryValues, mfLayer);
  d_pcrmf->setSecondaryStorage(secondaryValues, mfLayer);
}



void ModflowLink::setWetting(LinkInTransferArray linkInTransferArray){
  auto *values    =static_cast<float *>(linkInTransferArray[1]);
  int const mfLayer    = (static_cast<const int *>(linkInTransferArray[2]))[0];
  d_pcrmf->setWetting(values, mfLayer);

}

/**
 *
 */
void ModflowLink::setRecharge(LinkInTransferArray linkInTransferArray){
  auto *values = static_cast<float *>(linkInTransferArray[1]);
  int const rchCode = (static_cast<const int *>(linkInTransferArray[2]))[0];
  d_pcrmf->setRecharge(values, rchCode);
}

void ModflowLink::setIndicatedRecharge(LinkInTransferArray linkInTransferArray){
  auto *rch = static_cast<float *>(linkInTransferArray[1]);
  int *layer = static_cast<int *>(linkInTransferArray[2]);
  d_pcrmf->setRechargeLay(rch, layer);
}


void ModflowLink::getRecharge(LinkInTransferArray linkInTransferArray){
  auto *result = static_cast<float *>(linkInTransferArray[0]);
  int const mflayer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->getRecharge(result, mflayer);
}

void ModflowLink::setDrain(LinkInTransferArray linkInTransferArray){
  auto *elevation    =static_cast<float *>(linkInTransferArray[1]);
  auto *conductance = static_cast<float *>(linkInTransferArray[2]);
  int const mfLayer = (static_cast<const int *>(linkInTransferArray[3]))[0];
  d_pcrmf->setDrain(elevation, conductance, mfLayer);

}


void ModflowLink::getDrain(LinkInTransferArray linkInTransferArray){
  auto *result = static_cast<float *>(linkInTransferArray[0]);
  int const mflayer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->getDrain(result, mflayer);
}

//
// Well package
//
void ModflowLink::setWell(LinkInTransferArray linkInTransferArray){
  auto *well = static_cast<float *>(linkInTransferArray[1]);
  int const mfLayer = (static_cast<const int *>(linkInTransferArray[2]))[0];
  d_pcrmf->setWell(well, mfLayer);
}
//
// Solver packages
//
void ModflowLink::setSOR(LinkInTransferArray linkInTransferArray){
  int const mxiter = (static_cast<const int *>(linkInTransferArray[1]))[0];
  float const accl = (static_cast<const float *>(linkInTransferArray[2]))[0];
  float const hclose = (static_cast<const float *>(linkInTransferArray[3]))[0];
  d_pcrmf->setSOR(mxiter, accl, hclose);
}

void ModflowLink::setPCG(LinkInTransferArray linkInTransferArray){
  int const mxiter = (static_cast<const int *>(linkInTransferArray[1]))[0];
  int const iteri = (static_cast<const int *>(linkInTransferArray[2]))[0];
  int const npcond = (static_cast<const int *>(linkInTransferArray[3]))[0];
  float const hclose = (static_cast<const float *>(linkInTransferArray[4]))[0];
  float const rclose = (static_cast<const float *>(linkInTransferArray[5]))[0];
  float const relax = (static_cast<const float *>(linkInTransferArray[6]))[0];
  float const nbpol = (static_cast<const float *>(linkInTransferArray[7]))[0];
  float const damp = (static_cast<const float *>(linkInTransferArray[8]))[0];
  d_pcrmf->setPCG(mxiter, iteri, npcond, hclose, rclose, relax, nbpol, damp);
}


void ModflowLink::setSIP(LinkInTransferArray linkInTransferArray){
  int const mxiter = (static_cast<const int *>(linkInTransferArray[1]))[0];
  int const nparam = (static_cast<const int *>(linkInTransferArray[2]))[0];
  float const accl = (static_cast<const float *>(linkInTransferArray[3]))[0];
  float const hclose = (static_cast<const float *>(linkInTransferArray[4]))[0];
  int const ipcalc = (static_cast<const int *>(linkInTransferArray[5]))[0];
  float const wseed = (static_cast<const float *>(linkInTransferArray[6]))[0];
  d_pcrmf->setSIP(mxiter, nparam, accl, hclose, ipcalc, wseed);
}


void ModflowLink::setDSP(LinkInTransferArray linkInTransferArray){
  int const itmx = (static_cast<const int *>(linkInTransferArray[1]))[0];
  int const mxup = (static_cast<const int *>(linkInTransferArray[2]))[0];
  int const mxlow = (static_cast<const int *>(linkInTransferArray[3]))[0];
  int const mxbw = (static_cast<const int *>(linkInTransferArray[4]))[0];
  int const ifreq = (static_cast<const int *>(linkInTransferArray[5]))[0];
  float const accl = (static_cast<const float *>(linkInTransferArray[6]))[0];
  float const hclose = (static_cast<const float *>(linkInTransferArray[7]))[0];
  d_pcrmf->setDSP(itmx, mxup, mxlow, mxbw, ifreq, accl, hclose);
}




void ModflowLink::getStorage(LinkInTransferArray linkInTransferArray){
  auto *result = static_cast<float *>(linkInTransferArray[0]);
  int const layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->get_storage(result, layer);
}

void ModflowLink::getConstantHead(LinkInTransferArray linkInTransferArray){
  auto *result = static_cast<float *>(linkInTransferArray[0]);
  int const layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->get_constand_head(result, layer);
}

void ModflowLink::getRightFace(LinkInTransferArray linkInTransferArray){
  auto *result = static_cast<float *>(linkInTransferArray[0]);
  int const layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->get_right_face(result, layer);
}

void ModflowLink::getFrontFace(LinkInTransferArray linkInTransferArray){
  auto *result = static_cast<float *>(linkInTransferArray[0]);
  int const layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->get_front_face(result, layer);
}

void ModflowLink::getLowerFace(LinkInTransferArray linkInTransferArray){
  auto *result = static_cast<float *>(linkInTransferArray[0]);
  int const layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->get_lower_face(result, layer);
}




