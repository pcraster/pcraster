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
  auto *lower = (float *)linkInTransferArray[1];
  auto *upper = (float *)linkInTransferArray[2];
  d_pcrmf->createBottom(lower, upper);

}

/**
 * adding a layer on top of the grid
 */
void  ModflowLink::addLayer(LinkInTransferArray linkInTransferArray){
  auto *values = (float *)linkInTransferArray[1];
  d_pcrmf->addLayer(values);
}

/**
 * adding a confined layer on top of the grid
 */
void  ModflowLink::addConfinedLayer(LinkInTransferArray linkInTransferArray){
  auto *values = (float *)linkInTransferArray[1];
  d_pcrmf->addConfinedLayer(values);
}


void ModflowLink::setBoundary(LinkInTransferArray linkInTransferArray){
  int *values = (int *)linkInTransferArray[1];
  int const layer = ((const int *)linkInTransferArray[2])[0];
  d_pcrmf->setIBound(values, layer);
}


void ModflowLink::setHead(LinkInTransferArray linkInTransferArray){
  auto *values    =(float *)linkInTransferArray[1];
  int const layer    = ((const int *)linkInTransferArray[2])[0];
  d_pcrmf->setInitialHead(values, layer);
}


void ModflowLink::setConductivity(LinkInTransferArray linkInTransferArray){
  int const laycon    = ((const int *)linkInTransferArray[1])[0];
  auto *hConds    =(float *)linkInTransferArray[2];
  auto *vConds    =(float *)linkInTransferArray[3];
  int const layer    = ((const int *)linkInTransferArray[4])[0];

  d_pcrmf->setHCond(hConds, layer, laycon);
  d_pcrmf->setVCond(vConds, layer);
}



void ModflowLink::getHead(LinkInTransferArray linkInTransferArray){
  auto *result = (float *)linkInTransferArray[0];
  int const layer = ((const int *)linkInTransferArray[1])[0];
  d_pcrmf->getHeads(result, layer);
}

void ModflowLink::getRivLeak(LinkInTransferArray linkInTransferArray){
  auto *result = (float *)linkInTransferArray[0];
  int const layer = ((const int *)linkInTransferArray[1])[0];
  d_pcrmf->getRiverLeakage(result, layer);
}



void ModflowLink::setRiver(LinkInTransferArray linkInTransferArray){
  auto *rivH    =(float *)linkInTransferArray[1];
  auto *rivB    =(float *)linkInTransferArray[2];
  auto *rivC    =(float *)linkInTransferArray[3];
  int const mfLayer    = ((const int *)linkInTransferArray[4])[0];
  d_pcrmf->setRiver(rivH, rivB, rivC, mfLayer);
}


void ModflowLink::setDISParams(LinkInTransferArray linkInTransferArray){
  int const time = ((const int *)linkInTransferArray[1])[0];
  int const len = ((const int *)linkInTransferArray[2])[0];
  float const stresslen = ((const float *)linkInTransferArray[3])[0];
  int const tSteps = ((const int *)linkInTransferArray[4])[0];
  float const tMult = ((const float *)linkInTransferArray[5])[0];
  bool const ss = ((const bool *)linkInTransferArray[6])[0];
  d_pcrmf->setDISParams(time,len,stresslen,tSteps,tMult,ss);
}



void ModflowLink::setHNOFLO(LinkInTransferArray linkInTransferArray){
  const float val = ((const float *)linkInTransferArray[1])[0];
  d_pcrmf->setNoFlowConstant(val);
}



void ModflowLink::setTRPY(LinkInTransferArray linkInTransferArray){
  float const value = ((const float *)linkInTransferArray[1])[0];
  d_pcrmf->setTRPY(value);
}

void ModflowLink::setHDRY(LinkInTransferArray linkInTransferArray){
  float const value = ((const float *)linkInTransferArray[1])[0];
  d_pcrmf->setHDRY(value);
}

void ModflowLink::setWettingParameter(LinkInTransferArray linkInTransferArray){
  float const wetfct = ((const float *)linkInTransferArray[1])[0];
  int const iwetit = ((const int *)linkInTransferArray[2])[0];
  float const ihdwet = ((const float *)linkInTransferArray[3])[0];
  d_pcrmf->setWettingParameter(wetfct, iwetit, ihdwet);
}

void ModflowLink::setStorage(LinkInTransferArray linkInTransferArray){
  auto *primaryValues = (float *)linkInTransferArray[1];
  auto *secondaryValues = (float *)linkInTransferArray[2];
  int const mfLayer = ((const int *)linkInTransferArray[3])[0];
  d_pcrmf->setPrimaryStorage(primaryValues, mfLayer);
  d_pcrmf->setSecondaryStorage(secondaryValues, mfLayer);
}



void ModflowLink::setWetting(LinkInTransferArray linkInTransferArray){
  auto *values    =(float *)linkInTransferArray[1];
  int const mfLayer    = ((const int *)linkInTransferArray[2])[0];
  d_pcrmf->setWetting(values, mfLayer);

}

/**
 *
 */
void ModflowLink::setRecharge(LinkInTransferArray linkInTransferArray){
  auto *values = (float *)linkInTransferArray[1];
  int const rchCode = ((const int *)linkInTransferArray[2])[0];
  d_pcrmf->setRecharge(values, rchCode);
}

void ModflowLink::setIndicatedRecharge(LinkInTransferArray linkInTransferArray){
  auto *rch = (float *)linkInTransferArray[1];
  int *layer = (int *)linkInTransferArray[2];
  d_pcrmf->setRechargeLay(rch, layer);
}


void ModflowLink::getRecharge(LinkInTransferArray linkInTransferArray){
  auto *result = (float *)linkInTransferArray[0];
  int const mflayer = ((const int *)linkInTransferArray[1])[0];
  d_pcrmf->getRecharge(result, mflayer);
}

void ModflowLink::setDrain(LinkInTransferArray linkInTransferArray){
  auto *elevation    =(float *)linkInTransferArray[1];
  auto *conductance = (float *)linkInTransferArray[2];
  int const mfLayer = ((const int *)linkInTransferArray[3])[0];
  d_pcrmf->setDrain(elevation, conductance, mfLayer);

}


void ModflowLink::getDrain(LinkInTransferArray linkInTransferArray){
  auto *result = (float *)linkInTransferArray[0];
  int const mflayer = ((const int *)linkInTransferArray[1])[0];
  d_pcrmf->getDrain(result, mflayer);
}

//
// Well package
//
void ModflowLink::setWell(LinkInTransferArray linkInTransferArray){
  auto *well = (float *)linkInTransferArray[1];
  int const mfLayer = ((const int *)linkInTransferArray[2])[0];
  d_pcrmf->setWell(well, mfLayer);
}
//
// Solver packages
//
void ModflowLink::setSOR(LinkInTransferArray linkInTransferArray){
  int const mxiter = ((const int *)linkInTransferArray[1])[0];
  float const accl = ((const float *)linkInTransferArray[2])[0];
  float const hclose = ((const float *)linkInTransferArray[3])[0];
  d_pcrmf->setSOR(mxiter, accl, hclose);
}

void ModflowLink::setPCG(LinkInTransferArray linkInTransferArray){
  int const mxiter = ((const int *)linkInTransferArray[1])[0];
  int const iteri = ((const int *)linkInTransferArray[2])[0];
  int const npcond = ((const int *)linkInTransferArray[3])[0];
  float const hclose = ((const float *)linkInTransferArray[4])[0];
  float const rclose = ((const float *)linkInTransferArray[5])[0];
  float const relax = ((const float *)linkInTransferArray[6])[0];
  float const nbpol = ((const float *)linkInTransferArray[7])[0];
  float const damp = ((const float *)linkInTransferArray[8])[0];
  d_pcrmf->setPCG(mxiter, iteri, npcond, hclose, rclose, relax, nbpol, damp);
}


void ModflowLink::setSIP(LinkInTransferArray linkInTransferArray){
  int const mxiter = ((const int *)linkInTransferArray[1])[0];
  int const nparam = ((const int *)linkInTransferArray[2])[0];
  float const accl = ((const float *)linkInTransferArray[3])[0];
  float const hclose = ((const float *)linkInTransferArray[4])[0];
  int const ipcalc = ((const int *)linkInTransferArray[5])[0];
  float const wseed = ((const float *)linkInTransferArray[6])[0];
  d_pcrmf->setSIP(mxiter, nparam, accl, hclose, ipcalc, wseed);
}


void ModflowLink::setDSP(LinkInTransferArray linkInTransferArray){
  int const itmx = ((const int *)linkInTransferArray[1])[0];
  int const mxup = ((const int *)linkInTransferArray[2])[0];
  int const mxlow = ((const int *)linkInTransferArray[3])[0];
  int const mxbw = ((const int *)linkInTransferArray[4])[0];
  int const ifreq = ((const int *)linkInTransferArray[5])[0];
  float const accl = ((const float *)linkInTransferArray[6])[0];
  float const hclose = ((const float *)linkInTransferArray[7])[0];
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




