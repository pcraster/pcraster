#ifndef INCLUDED_PCRMODFLOWLINK
#include "pcr_mflink.h"
#define INCLUDED_PCRMODFLOWLINK
#endif


ModflowLink::ModflowLink(pcrxml::LinkInExecuteInput const& l){
  size_t rows = l.context().areaMap().nrRows();
  size_t cols = l.context().areaMap().nrCols();

  xsd::cxx::tree::optional<double> tmpCells(l.context().areaMap().cellSize());
  xsd::cxx::tree::optional<double> tmpXll(l.context().areaMap().xLowerLeftCorner());
  xsd::cxx::tree::optional<double> tmpYll(l.context().areaMap().yLowerLeftCorner());

  double cells = tmpCells.get();
  double west = tmpXll.get();
  double north = tmpYll.get();

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
  float *lower = (float *)linkInTransferArray[1];
  float *upper = (float *)linkInTransferArray[2];
  d_pcrmf->createBottom(lower, upper);

}

/**
 * adding a layer on top of the grid
 */
void  ModflowLink::addLayer(LinkInTransferArray linkInTransferArray){
  float *values = (float *)linkInTransferArray[1];
  d_pcrmf->addLayer(values);
}

/**
 * adding a confined layer on top of the grid
 */
void  ModflowLink::addConfinedLayer(LinkInTransferArray linkInTransferArray){
  float *values = (float *)linkInTransferArray[1];
  d_pcrmf->addConfinedLayer(values);
}


void ModflowLink::setBoundary(LinkInTransferArray linkInTransferArray){
  int *values = (int *)linkInTransferArray[1];
  int layer = ((const int *)linkInTransferArray[2])[0];
  d_pcrmf->setIBound(values, layer);
}


void ModflowLink::setHead(LinkInTransferArray linkInTransferArray){
  float *values    =(float *)linkInTransferArray[1];
  int layer    = ((const int *)linkInTransferArray[2])[0];
  d_pcrmf->setInitialHead(values, layer);
}


void ModflowLink::setConductivity(LinkInTransferArray linkInTransferArray){
  int laycon    = ((const int *)linkInTransferArray[1])[0];
  float *hConds    =(float *)linkInTransferArray[2];
  float *vConds    =(float *)linkInTransferArray[3];
  int layer    = ((const int *)linkInTransferArray[4])[0];

  d_pcrmf->setHCond(hConds, layer, laycon);
  d_pcrmf->setVCond(vConds, layer);
}



void ModflowLink::getHead(LinkInTransferArray linkInTransferArray){
  float *result = (float *)linkInTransferArray[0];
  int layer = ((const int *)linkInTransferArray[1])[0];
  d_pcrmf->getHeads(result, layer);
}

void ModflowLink::getRivLeak(LinkInTransferArray linkInTransferArray){
  float *result = (float *)linkInTransferArray[0];
  int layer = ((const int *)linkInTransferArray[1])[0];
  d_pcrmf->getRiverLeakage(result, layer);
}



void ModflowLink::setRiver(LinkInTransferArray linkInTransferArray){
  float *rivH    =(float *)linkInTransferArray[1];
  float *rivB    =(float *)linkInTransferArray[2];
  float *rivC    =(float *)linkInTransferArray[3];
  int mfLayer    = ((const int *)linkInTransferArray[4])[0];
  d_pcrmf->setRiver(rivH, rivB, rivC, mfLayer);
}


void ModflowLink::setDISParams(LinkInTransferArray linkInTransferArray){
  int time = ((const int *)linkInTransferArray[1])[0];
  int len = ((const int *)linkInTransferArray[2])[0];
  float stresslen = ((const float *)linkInTransferArray[3])[0];
  int tSteps = ((const int *)linkInTransferArray[4])[0];
  float tMult = ((const float *)linkInTransferArray[5])[0];
  bool ss = ((const bool *)linkInTransferArray[6])[0];
  d_pcrmf->setDISParams(time,len,stresslen,tSteps,tMult,ss);
}



void ModflowLink::setHNOFLO(LinkInTransferArray linkInTransferArray){
  const float val = ((const float *)linkInTransferArray[1])[0];
  d_pcrmf->setNoFlowConstant(val);
}



void ModflowLink::setTRPY(LinkInTransferArray linkInTransferArray){
  float value = ((const float *)linkInTransferArray[1])[0];
  d_pcrmf->setTRPY(value);
}

void ModflowLink::setHDRY(LinkInTransferArray linkInTransferArray){
  float value = ((const float *)linkInTransferArray[1])[0];
  d_pcrmf->setHDRY(value);
}

void ModflowLink::setWettingParameter(LinkInTransferArray linkInTransferArray){
  float wetfct = ((const float *)linkInTransferArray[1])[0];
  int iwetit = ((const int *)linkInTransferArray[2])[0];
  float ihdwet = ((const float *)linkInTransferArray[3])[0];
  d_pcrmf->setWettingParameter(wetfct, iwetit, ihdwet);
}

void ModflowLink::setStorage(LinkInTransferArray linkInTransferArray){
  float *primaryValues = (float *)linkInTransferArray[1];
  float *secondaryValues = (float *)linkInTransferArray[2];
  int mfLayer = ((const int *)linkInTransferArray[3])[0];
  d_pcrmf->setPrimaryStorage(primaryValues, mfLayer);
  d_pcrmf->setSecondaryStorage(secondaryValues, mfLayer);
}



void ModflowLink::setWetting(LinkInTransferArray linkInTransferArray){
  float *values    =(float *)linkInTransferArray[1];
  int mfLayer    = ((const int *)linkInTransferArray[2])[0];
  d_pcrmf->setWetting(values, mfLayer);

}

/**
 *
 */
void ModflowLink::setRecharge(LinkInTransferArray linkInTransferArray){
  float *values = (float *)linkInTransferArray[1];
  int rchCode = ((const int *)linkInTransferArray[2])[0];
  d_pcrmf->setRecharge(values, rchCode);
}

void ModflowLink::setIndicatedRecharge(LinkInTransferArray linkInTransferArray){
  float *rch = (float *)linkInTransferArray[1];
  int *layer = (int *)linkInTransferArray[2];
  d_pcrmf->setRechargeLay(rch, layer);
}


void ModflowLink::getRecharge(LinkInTransferArray linkInTransferArray){
  float *result = (float *)linkInTransferArray[0];
  int mflayer = ((const int *)linkInTransferArray[1])[0];
  d_pcrmf->getRecharge(result, mflayer);
}

void ModflowLink::setDrain(LinkInTransferArray linkInTransferArray){
  float *elevation    =(float *)linkInTransferArray[1];
  float *conductance = (float *)linkInTransferArray[2];
  int mfLayer = ((const int *)linkInTransferArray[3])[0];
  d_pcrmf->setDrain(elevation, conductance, mfLayer);

}


void ModflowLink::getDrain(LinkInTransferArray linkInTransferArray){
  float *result = (float *)linkInTransferArray[0];
  int mflayer = ((const int *)linkInTransferArray[1])[0];
  d_pcrmf->getDrain(result, mflayer);
}

//
// Well package
//
void ModflowLink::setWell(LinkInTransferArray linkInTransferArray){
  float *well = (float *)linkInTransferArray[1];
  int mfLayer = ((const int *)linkInTransferArray[2])[0];
  d_pcrmf->setWell(well, mfLayer);
}
//
// Solver packages
//
void ModflowLink::setSOR(LinkInTransferArray linkInTransferArray){
  int mxiter = ((const int *)linkInTransferArray[1])[0];
  float accl = ((const float *)linkInTransferArray[2])[0];
  float hclose = ((const float *)linkInTransferArray[3])[0];
  d_pcrmf->setSOR(mxiter, accl, hclose);
}

void ModflowLink::setPCG(LinkInTransferArray linkInTransferArray){
  int mxiter = ((const int *)linkInTransferArray[1])[0];
  int iteri = ((const int *)linkInTransferArray[2])[0];
  int npcond = ((const int *)linkInTransferArray[3])[0];
  float hclose = ((const float *)linkInTransferArray[4])[0];
  float rclose = ((const float *)linkInTransferArray[5])[0];
  float relax = ((const float *)linkInTransferArray[6])[0];
  float nbpol = ((const float *)linkInTransferArray[7])[0];
  float damp = ((const float *)linkInTransferArray[8])[0];
  d_pcrmf->setPCG(mxiter, iteri, npcond, hclose, rclose, relax, nbpol, damp);
}


void ModflowLink::setSIP(LinkInTransferArray linkInTransferArray){
  int mxiter = ((const int *)linkInTransferArray[1])[0];
  int nparam = ((const int *)linkInTransferArray[2])[0];
  float accl = ((const float *)linkInTransferArray[3])[0];
  float hclose = ((const float *)linkInTransferArray[4])[0];
  int ipcalc = ((const int *)linkInTransferArray[5])[0];
  float wseed = ((const float *)linkInTransferArray[6])[0];
  d_pcrmf->setSIP(mxiter, nparam, accl, hclose, ipcalc, wseed);
}


void ModflowLink::setDSP(LinkInTransferArray linkInTransferArray){
  int itmx = ((const int *)linkInTransferArray[1])[0];
  int mxup = ((const int *)linkInTransferArray[2])[0];
  int mxlow = ((const int *)linkInTransferArray[3])[0];
  int mxbw = ((const int *)linkInTransferArray[4])[0];
  int ifreq = ((const int *)linkInTransferArray[5])[0];
  float accl = ((const float *)linkInTransferArray[6])[0];
  float hclose = ((const float *)linkInTransferArray[7])[0];
  d_pcrmf->setDSP(itmx, mxup, mxlow, mxbw, ifreq, accl, hclose);
}




void ModflowLink::getStorage(LinkInTransferArray linkInTransferArray){
  float *result = static_cast<float *>(linkInTransferArray[0]);
  int layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->get_storage(result, layer);
}

void ModflowLink::getConstantHead(LinkInTransferArray linkInTransferArray){
  float *result = static_cast<float *>(linkInTransferArray[0]);
  int layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->get_constand_head(result, layer);
}

void ModflowLink::getRightFace(LinkInTransferArray linkInTransferArray){
  float *result = static_cast<float *>(linkInTransferArray[0]);
  int layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->get_right_face(result, layer);
}

void ModflowLink::getFrontFace(LinkInTransferArray linkInTransferArray){
  float *result = static_cast<float *>(linkInTransferArray[0]);
  int layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->get_front_face(result, layer);
}

void ModflowLink::getLowerFace(LinkInTransferArray linkInTransferArray){
  float *result = static_cast<float *>(linkInTransferArray[0]);
  int layer = (static_cast<const int *>(linkInTransferArray[1]))[0];
  d_pcrmf->get_lower_face(result, layer);
}




