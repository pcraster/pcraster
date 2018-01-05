#ifndef INCLUDED_PCRMODFLOW
#include "pcrmodflow.h"
#define INCLUDED_PCRMODFLOW
#endif


// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_IOS
#include <ios>
#define INCLUDED_IOS
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#include <boost/filesystem/operations.hpp>
#define INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#endif

#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif

#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif


// PCRaster library headers.
#ifndef INCLUDED_DAL_DAL
#include "dal_Dal.h"
#define INCLUDED_DAL_DAL
#endif

#ifndef INCLUDED_DAL_RASTERDAL
#include "dal_RasterDal.h"
#define INCLUDED_DAL_RASTERDAL
#endif

#ifndef INCLUDED_DAL_LIBRARY
#include "dal_Library.h"
#define INCLUDED_DAL_LIBRARY
#endif

#ifndef INCLUDED_DEV_FILESYSTEMUTILS
#include "dev_FilesystemUtils.h"
#define INCLUDED_DEV_FILESYSTEMUTILS
#endif

// Module headers.
#ifndef INCLUDED_GRIDCHECK
#include "gridcheck.h"
#define INCLUDED_GRIDCHECK
#endif

#ifndef INCLUDED_WEL
#include "wel.h"
#define INCLUDED_WEL
#endif

#ifndef INCLUDED_DRN
#include "drn.h"
#define INCLUDED_DRN
#endif

#ifndef INCLUDED_RCH
#include "rch.h"
#define INCLUDED_RCH
#endif

#ifndef INCLUDED_BAS
#include "bas.h"
#define INCLUDED_BAS
#endif

#ifndef INCLUDED_BCF
#include "bcf.h"
#define INCLUDED_BCF
#endif

#ifndef INCLUDED_COMMON
#include "common.h"
#define INCLUDED_COMMON
#endif

#ifndef INCLUDED_PCG
#include "pcg.h"
#define INCLUDED_PCG
#endif

#ifndef INCLUDED_SIP
#include "sip.h"
#define INCLUDED_SIP
#endif

#ifndef INCLUDED_SOR
#include "sor.h"
#define INCLUDED_SOR
#endif

#ifndef INCLUDED_DSP
#include "dsp.h"
#define INCLUDED_DSP
#endif

#ifndef INCLUDED_RIV
#include "riv.h"
#define INCLUDED_RIV
#endif

#ifndef INCLUDED_DIS
#include "dis.h"
#define INCLUDED_DIS
#endif

#ifndef INCLUDED_HFB
#include "hfb.h"
#define INCLUDED_HFB
#endif

#include "mf_utils.h"

#include <QProcess>
#include <QString>

/// \todo change the confined level vector to layer vector


/**
 * Destructor
 */
PCRModflow::~PCRModflow() {
  resetGrid(true);
}

/**
 * Constructor for Python
 * spatial dimensions passed by clone()
 */

PCRModflow::PCRModflow(const geo::RasterSpace &raster)
  : dal::Client("",false)
{
  d_nrOfRows = raster.nrRows();
  d_nrOfColumns = raster.nrCols();

  d_nrOfCells = d_nrOfRows * d_nrOfColumns;
  d_west = raster.west();
  d_north = raster.north();


  d_widthRows = raster.cellSize();
  d_widthColumns = raster.cellSize();
  d_cellsize = raster.cellSize();
  initDataStructures();
  //
  if(d_nrOfRows == 0){
    d_cmethods->error("Aremap : No rows specified", "initialise");
    }
  if(d_nrOfColumns == 0){
    d_cmethods->error("Aremap : No columns specified", "initialise");
    }
  d_draster = new discr::Raster(d_nrOfRows, d_nrOfColumns, d_widthRows, d_west, d_north);
  d_thisbaseelev = new discr::RasterData<REAL4>(d_draster);
}

/**
 * Constructor
 * arguments are spatial properties retrieved from the linkIn
 */
PCRModflow::PCRModflow(size_t nrRows, size_t nrCols, double cellsize, double west, double north)
  : dal::Client("", false)
{
  d_nrOfRows = nrRows;
  d_nrOfColumns = nrCols;

  d_nrOfCells = d_nrOfRows * d_nrOfColumns;
  d_west = west;
  d_north = north;

  d_widthRows = cellsize;
  d_widthColumns = cellsize;
  d_cellsize = cellsize;
  initDataStructures();
  //
  if(d_nrOfRows == 0){
    d_cmethods->error("Clone map: No rows specified", "initialise");
    }
  if(d_nrOfColumns == 0){
    d_cmethods->error("Clone map: No columns specified", "initialise");
    }
  d_draster = new discr::Raster(d_nrOfRows, d_nrOfColumns, d_widthRows, d_west, d_north);
  d_thisbaseelev = new discr::RasterData<REAL4>(d_draster);
}


/**
 * spatial attributes for the raster
 * \todo check how it works with the python version, can be
 *
 */
void PCRModflow::initDataStructures(){
  assert(dal::Client::isInitialized());

  d_modflow_converged = true;
  d_solver_used = NO_SOLVER;


  d_nrOfLayer = -1;
  d_baseElev = true;
  d_baseLayer = NULL;
  d_nrMFLayer = 0;
  d_nrBlockLayer = 0;
  d_gridIsFixed = false;
  d_lastIsConfined = false;
  d_solver = false;
  d_isSteadyState = true;
  d_ibound = NULL;
  d_baseArea = NULL;
  d_initialHead = NULL;
  d_layer = NULL;
  d_hCond = NULL;
  d_vCond = NULL;

  d_dis = NULL;
  d_bas = NULL;
  d_bcf = NULL;

  d_riv = NULL;
  d_rivStage = NULL;
  d_rivBottom = NULL;
  d_rivCond = NULL;


  d_primaryStorage = NULL;
  d_secondaryStorage = NULL;
  d_rch = NULL;
  d_recharge = NULL;
  d_rechargeIrch = NULL;
 // d_rechargeResult = NULL;
  d_wetting = NULL;
  // DRN
  d_drn = NULL;
  d_drnElev = NULL;
  d_drnCond = NULL;
//  d_drnResult = NULL;
  // solver
  d_pcg = NULL;
  d_sip = NULL;
  d_sor = NULL;
  d_dsp = NULL;
  // well
  d_wel = NULL;
  d_welValues = NULL;

  dd_nrLayer = 0;
  dd_nrModflowLayer = 0;

  d_cmethods = new Common(this);
  d_gridCheck = new GridCheck(this);

  modflow_directory = "";
  run_command = "";
  run_arguments = "";
}



/**
 * resetting values in case of grid change
 */
void PCRModflow::resetGrid(bool final) {
  d_nrOfLayer = -1;
  d_nrOfCells = -1;
  d_nrMFLayer = 0;
  d_nrBlockLayer = 0;
  //d_firstLayer = true;
  d_baseElev = true;
  d_gridIsFixed = false;
  d_lastIsConfined = false;
  d_isSteadyState = true;
  d_solver = false;
  d_quasiConfined.clear();
  d_layer2BlockLayer.clear();
  d_layerType.clear();
  if(d_pcg!=NULL){
    delete d_pcg;
    d_pcg= NULL;
  }
  if(d_sip!=NULL){
    delete d_sip;
    d_sip = NULL;
  }
  if(d_sor!=NULL){
    delete d_sor;
    d_sor = NULL;
  }
  delete d_dsp;
  d_dsp = NULL;
   if(d_riv!=NULL){
//   delete d_rivLeakage;
//   d_rivLeakage = NULL;
    delete d_rivStage;
    d_rivStage = NULL;
    delete d_rivBottom;
    d_rivBottom = NULL;
    delete d_rivCond;
    d_rivCond = NULL;
    delete d_riv;
    d_riv = NULL;
  }
  if(d_initialHead!=NULL){
    delete d_initialHead;
    d_initialHead = NULL;
  }
  if(d_ibound!=NULL){
    delete d_ibound;
    d_ibound = NULL;
  }

  delete d_hCond;
  d_hCond = NULL;
  delete d_vCond;
  d_vCond = NULL;

  delete d_dis;
  d_dis = NULL;
  delete d_layer;
  d_layer = NULL;
  delete d_bas;
  d_bas = NULL;
  delete d_bcf;
  d_bcf = NULL;
  if(d_rch != NULL){
    delete d_rch;
    delete d_recharge;
    //delete d_rechargeResult;
    d_rch = NULL;
    d_recharge = NULL;
    //d_rechargeResult = NULL;
    delete d_rechargeIrch;
    d_rechargeIrch = NULL;
  }
  if(d_drn != NULL){
    delete d_drn;
    delete d_drnElev;
    delete d_drnCond;
   // delete d_drnResult;
    d_drn = NULL;
    d_drnElev = NULL;
    d_drnCond = NULL;
    //d_drnResult = NULL;
  }
  if(d_primaryStorage != NULL){
    delete d_primaryStorage;
    delete d_secondaryStorage;
    d_primaryStorage = NULL;
    d_secondaryStorage = NULL;
  }
  delete d_cmethods;
  d_cmethods = NULL;

  delete d_draster;
  d_draster = NULL;
  delete d_thisbaseelev;
  d_thisbaseelev = NULL;
  if(d_wetting != NULL){
    delete d_wetting;
    d_wetting = NULL;
  }
  if(d_wel != NULL){
    delete d_wel;
    delete d_welValues;
    d_wel = NULL;
    d_welValues = NULL;
  }
  delete d_baseLayer;
  d_baseLayer = NULL;

  delete d_gridCheck;
  d_gridCheck = NULL;

  delete d_baseArea;
  d_baseArea = NULL;

  if(final == false) {
    d_cmethods = new Common(this);
    d_gridCheck = new GridCheck(this);
  }
}


int PCRModflow::nr_internal_layer() const {
  return d_nrBlockLayer;
}

int PCRModflow::nr_modflow_layer() const {
  return d_nrMFLayer;
}

calc::Field* PCRModflow::getHeads(size_t mfLayer){
  return d_bas->getHeads(mfLayer);
}



void PCRModflow::getHeads(float *result, size_t mfLayer){
  d_bas->getHeads(result, mfLayer);
}

discr::BlockData<REAL4>* PCRModflow::getBlockHeads(){
  return d_bas->getHeads();
}



void PCRModflow::getRecharge(float *result, size_t mfLayer){
  if(NULL == d_rch) {
    std::stringstream stmp;
    stmp << "No recharge package specified ";
    d_cmethods->error(stmp.str(), "getRecharge");
  }
  d_rch->getRecharge(result, mfLayer, run_directory());
}

calc::Field* PCRModflow::getRecharge(size_t layer){
  if(NULL == d_rch) {
    std::stringstream stmp;
    stmp << "No recharge package specified ";
    d_cmethods->error(stmp.str(), "getRecharge");
  }
  return d_rch->getRecharge(layer, run_directory());
}

// discr::BlockData<REAL4>* PCRModflow::getBlockDrain(){
//   return d_drn->getBlockCellByCellFlow();
// }

// discr::BlockData<REAL4>* PCRModflow::getBlockRecharge(){
//   return d_rch->getBlockCellByCellFlow();
// }

// discr::BlockData<REAL4>* PCRModflow::getBlockRiverLeakage(){
//   return d_riv->getBlockCellByCellFlow();
// }

/**
 * \todo to riv
 */
void PCRModflow::getRiverLeakage(float *result, size_t layer){
  if(NULL == d_riv) {
    std::stringstream stmp;
    stmp << "No river package specified: Define river head, bottom and conductance values ";
    d_cmethods->error(stmp.str(), "getRiverLeakage");
  }
  d_riv->getRiverLeakage(result, layer, run_directory());
}

calc::Field* PCRModflow::getRiverLeakage(size_t layer){
  if(NULL == d_riv) {
    std::stringstream stmp;
    stmp << "No river package specified: Define river head, bottom and conductance values ";
    d_cmethods->error(stmp.str(), "getRiverLeakage");
  }
  return d_riv->getRiverLeakage(layer, run_directory());
}


/**
 * sets the bottom layer
 */
bool PCRModflow::createBottom(const float *lower, const float *upper){
  initBlockData();
  return d_dis->createBottom(lower, upper);
}

/**
 * adds unconfined layer
 */
bool PCRModflow::addLayer(const float *values){
  return d_dis->addLayer(values);
}

/**
 * adds layer representing a confining bed
 */
bool PCRModflow::addConfinedLayer(const float *values){
  return d_dis->addConfinedLayer(values);
}




bool PCRModflow::setBlockData(discr::BlockData<INT4> &bdata, const INT4 *values, size_t blockLayer) {
  d_gridCheck->testMV(values, d_methodName);
  for(size_t i=0; i < d_nrOfCells; i++) {
    assert(blockLayer < bdata.cell(i).size());
    bdata.cell(i)[blockLayer] = values[i];
  }
  return true;
}


bool PCRModflow::setBlockData(discr::BlockData<REAL4> &bdata, const REAL4 *values, size_t blockLayer) {
  d_gridCheck->testMV(values, d_methodName);
  for(size_t i = 0; i < d_nrOfCells; i++) {
    assert(blockLayer < bdata.cell(i).size());
    bdata.cell(i)[blockLayer] = values[i];
  }
  return true;
}


/**
 * writing NAM to file
 */
bool PCRModflow::writeNAM() {
  std::stringstream content;
  // the unit numbers are just assigned manually
  // 0,5,6,100,101 seem to be sometimes used
  // by compilers for stdout/stderr,...
  //
  // from 230 the
  //
  content << "# Generated by PCRaster Modflow\n";
  content << "# Output files\n";
  content << "LIST  206 pcrmf.lst\n";
  content << "# Input files\n";


  content << "BAS6  207 pcrmf.ba6\n";
  content << "DIS   208 pcrmf.dis\n";
  content << "DATA  300 pcrmf_elev.asc\n";
  content << "DATA  400 pcrmf_heads.asc\n";
  content << "DATA  401 pcrmf_bounds.asc\n";

  content << "BCF6  209 pcrmf.bc6\n";
  content << "DATA  " << d_bcf->hy_unit_number() << " pcrmf_bcf_hy.asc\n";
  content << "DATA  " << d_bcf->vcond_unit_number() << " pcrmf_bcf_vcont.asc\n";
  content << "DATA  " << d_bcf->tran_unit_number() << " pcrmf_bcf_tran.asc\n";

  if(d_bcf->transient()){
    content << "DATA  " << d_bcf->sf1_unit_number() << " pcrmf_bcf_sf1.asc\n";
    content << "DATA  " << d_bcf->sf2_unit_number() << " pcrmf_bcf_sf2.asc\n";
  }

  if(d_bcf->rewetting()){
    content << "DATA  " << d_bcf->wet_unit_number() << " pcrmf_bcf_wetdry.asc\n";
  }






  if(d_solver_used == PCG_SOLVER) {
    content << "PCG   210 pcrmf.pcg\n";
  }
  if(d_solver_used == SIP_SOLVER) {
    content << "SIP   210 pcrmf.sip\n";
  }
  if(d_solver_used == SOR_SOLVER) {
    content << "SOR   210 pcrmf.sor\n";
  }
  if(d_solver_used == DSP_SOLVER) {
    content << "DE4   210 pcrmf.de4\n";
  }
  content << "OC    220 pcrmf.oc\n";
  if(d_riv!=NULL){
    content << "RIV   221 pcrmf.riv\n";
    content << "DATA  251 pcrmf_riv.asc\n";
  }

  if(d_rch!=NULL){
    content << "RCH   222 pcrmf.rch\n";
    content << "DATA  261 pcrmf_rch.asc\n";
    if(d_rch->indicated_recharge()){
      content << "DATA  262 pcrmf_irch.asc\n";
    }
  }

  if(d_drn != NULL){
    content << "DRN   223 pcrmf.drn\n";
    content << "DATA  270 pcrmf_drn.asc\n";
  }

  if(d_wel != NULL) {
    content << "WEL   224 pcrmf.wel\n";
    content << "DATA  280 pcrmf_wel.asc\n";
  }


  return d_cmethods->writeToFile(mf::execution_path(run_directory(), "pcrmf.nam"), content.str());
}

/**
 * writing OC to file
 */
bool PCRModflow::writeOC() {
  std::stringstream content;
  content << "# Generated by PCRaster Modflow\n";
  content << "HEAD SAVE UNIT " << d_bas->fortran_unit_number_heads() << "\n";
  content << "IBOUND SAVE UNIT " << d_bas->fortran_unit_number_bounds() << "\n";
  content << "PERIOD 1 STEP " << d_dis->getTimeSteps() << "\n";
  content << "SAVE HEAD" << "\n";
  content << "SAVE IBOUND" << "\n";
  content << "SAVE BUDGET" << "\n";
  return d_cmethods->writeToFile(mf::execution_path(run_directory(), "pcrmf.oc"), content.str());
}



void PCRModflow::initBlock(const discr::Block &elevation) {
  //d_thisbaseelev = new discr::RasterData<REAL4>(d_draster);
  for(size_t i = 0; i < d_nrOfCells; i++) {
    d_thisbaseelev->cell(i) = elevation.cell(i).baseElevation();
  }
  initBlockData();
}


void PCRModflow::initBlockData() {
  d_baseArea = new discr::Block(*d_thisbaseelev);
  d_ibound = new discr::BlockData<INT4>(d_baseArea, 0);
  const REAL4 init = 0.0;

  initREAL4BlockData(&d_initialHead, init);
  initREAL4BlockData( &d_hCond, init);
  initREAL4BlockData( &d_vCond, init);
  initREAL4BlockData( &d_layer, init);
  initREAL4BlockData( &d_baseLayer, init);

  d_dis = new DIS(this);
  d_bas = new BAS(this);
  d_bcf = new BCF(this);
}


void PCRModflow::setLayer(const discr::Block &elevation, const discr::BlockData<INT4> &conf) {
  if(d_gridIsFixed == true) {
    resetGrid(false);
    d_gridIsFixed = false;
  }
  initBlock(elevation);
  d_dis->setLayer(elevation, conf);
}

void PCRModflow::createBottom(const calc::Field *lower, const calc::Field *upper){
  initBlockData();
  d_dis->createBottom(lower, upper);
}
void PCRModflow::addLayer(const calc::Field *values){
  d_dis->addLayer(values);
}
void PCRModflow::addConfinedLayer(const calc::Field *values){
  d_dis->addConfinedLayer(values);
}




size_t PCRModflow::mfLayer2BlockLayer(size_t mflayer) {
  mflayer++; // mflayer are
  try {
    size_t size = d_layer2BlockLayer.size();
    return d_layer2BlockLayer.at(size-mflayer);
  }
  catch(std::exception const& e) {
    std::cout << "mfLayer2BlockLayer " << mflayer << " "<< e.what() << std::endl;
    exit(1);
  }
}


/**
 * setting the boundary values
 * @param values boundary values
 * @param layer layer number
 */
bool PCRModflow::setIBound(const int *values, size_t layer) {
  layer--; // layer number passed by user starts with 1
  d_gridCheck->isGrid(layer, "setBoundary");
  d_gridCheck->isConfined(layer, "setBoundary");
  d_gridCheck->testMV(values, "setBoundary");
  // size_t blockLayer = mfLayer2BlockLayer(mfLayer);
  bool result = setBlockData(*d_ibound, values, layer);
  return result;
}

bool PCRModflow::setIBound(const discr::BlockData<INT4> &values) {
  d_bas->setBASBlockData(values, *d_ibound);
  return true;
}

void PCRModflow::setIBound(const calc::Field *values, size_t layer){
  d_bas->setIBound(values, layer);
}
void PCRModflow::setInitialHead(const calc::Field *values, size_t layer){

  if(d_bas == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setInitialHead");
  }

  d_bas->setInitialHead(values, layer);
}


void PCRModflow::initRIV() {
  const REAL4 init = 0.0;
  d_riv = new RIV(this);

  initREAL4BlockData(&d_rivStage, init);
  initREAL4BlockData(&d_rivBottom, init);
  initREAL4BlockData(&d_rivCond, init);
 // initREAL4BlockData(&d_rivLeakage, init);
}

void PCRModflow::initREAL4BlockData(discr::BlockData<REAL4> **bdata, double init) {
  *bdata = new discr::BlockData<REAL4>(d_baseArea, init);
}

void PCRModflow::setRiver(const calc::Field *rivH, const calc::Field *rivB, const calc::Field *rivC, size_t layer){
  //if(d_gridIsFixed == true){
  //  resetGrid(false);
  //  d_gridIsFixed = false;
  //}
  if(d_riv == NULL) {
    initRIV();
  }
  d_riv->setRiver(rivH, rivB, rivC, layer);
}


bool PCRModflow::setRiver(const float *rivH, const float *rivB, const float *rivC, size_t layer) {
  //if(d_gridIsFixed == true){
  //  resetGrid(false);
  //  d_gridIsFixed = false;
  //}
  if(d_riv == NULL) {
    initRIV();
  }
  bool result = d_riv->setRiver(rivH, rivB, rivC, layer);/*
  layer--; // layer number passed by user starts with 1
  d_gridCheck->isGrid(layer, "setRiver");
  d_gridCheck->isConfined(layer, "setRiver");
  d_methodName = "setRiver head values";
  //size_t layer = mfLayer2BlockLayer(mfLayer);
  bool result = setBlockData(*d_rivStage, rivH, layer);
  d_methodName = "setRiver bottom values";
  result = setBlockData(*d_rivBottom, rivB, layer);
  d_methodName = "setRiver conductance values";
  result = setBlockData(*d_rivCond, rivC, layer);*/
  return result;
}

void PCRModflow::setRiver(discr::BlockData<REAL4> &stage, discr::BlockData<REAL4> &bottom, discr::BlockData<REAL4> &cond) {
  //if(d_gridIsFixed == true) {
  //  resetGrid(false);
  //  d_gridIsFixed = false;
  //}
  if(d_riv == NULL) {
    initRIV();
  }
  d_riv->setRiver(stage, bottom, cond);
}


/**
 * setting the initial head
 * @param values initial heads
 * @param layer layer number
 */
bool PCRModflow::setInitialHead(const float *values, size_t layer) {
  layer--; // layer number passed by user starts with 1
  d_gridCheck->isGrid(layer, "setInitialHead");
  d_gridCheck->isConfined(layer, "setInitialHead");
  d_gridCheck->testMV(values, "setInitialHead");
  //size_t blockLayer = mfLayer2BlockLayer(mfLayer);
  bool result = setBlockData(*d_initialHead, values, layer);
  return result;
}


bool PCRModflow::setInitialHead(const discr::BlockData<REAL4> &values){

  if(d_bas == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setInitialHead");
  }
  d_bas->setBASBlockData(values, *d_initialHead);
  return true;
}


void PCRModflow::setStorage(const calc::Field *primary, const calc::Field *secondary, size_t layer){

  if(d_bcf == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setStorage");
  }

  if(d_primaryStorage == NULL) {
    d_primaryStorage = new discr::BlockData<REAL4>(d_baseArea);
    d_secondaryStorage = new discr::BlockData<REAL4>(d_baseArea);
  }
  d_bcf->setStorage(primary, secondary, layer);
}

/**
 * setting the primary storage values
 * @param values storage values
 * @param layer layer number
 */
bool PCRModflow::setPrimaryStorage(const float *values, size_t layer){
  if(d_primaryStorage == NULL) {
    d_primaryStorage = new discr::BlockData<REAL4>(d_baseArea);
    d_secondaryStorage = new discr::BlockData<REAL4>(d_baseArea);
  }
  layer--; // layer number passed by user starts with 1
  d_gridCheck->isGrid(layer, "setPrimaryStorage");
  d_gridCheck->isConfined(layer, "setStorage");
  d_gridCheck->testMV(values, "setPrimaryStorage");
  bool result = setBlockData(*d_primaryStorage, values, layer);
  return result;
}

/**
 * setting the secondary storage values
 * @param values storage values
 * @param layer layer number
 */
bool PCRModflow::setSecondaryStorage(const float *values, size_t layer){
  if(d_primaryStorage == NULL) {
    d_primaryStorage = new discr::BlockData<REAL4>(d_baseArea);
    d_secondaryStorage = new discr::BlockData<REAL4>(d_baseArea);
  }
  layer--; // layer number passed by user starts with 1
  d_gridCheck->isGrid(layer, "setSecondaryStorage");
  d_gridCheck->isConfined(layer, "setStorage");
  d_gridCheck->testMV(values, "setSecondaryStorage");
  bool result = setBlockData(*d_secondaryStorage, values, layer);
  return result;
}


void PCRModflow::setWettingParameter(float wetfct, size_t iwetit, float ihdwet){

  if(d_bcf == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setWettingParameter");
  }

  if(d_wetting == NULL) {
    d_wetting = new discr::BlockData<REAL4>(d_baseArea);
  }
  d_bcf->setWettingParameter(wetfct, iwetit, ihdwet);
}

/**
 * setting the wetting values
 * @param values storage values
 * @param layer layer number
 */
bool PCRModflow::setWetting(const float *values, size_t layer){
  if(d_wetting == NULL) {
    d_wetting = new discr::BlockData<REAL4>(d_baseArea);
  }
  layer--; // layer number passed by user starts with 1
  d_gridCheck->isGrid(layer, "setWetting");
  d_gridCheck->isConfined(layer, "setWetting");
  d_gridCheck->testMV(values, "setWetting");
  bool result = setBlockData(*d_wetting, values, layer);
  return result;
}

void PCRModflow::setWetting(const calc::Field *values, size_t layer){

  if(d_bcf == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setWetting");
  }

  if(d_wetting == NULL) {
    d_wetting = new discr::BlockData<REAL4>(d_baseArea);
  }
  d_bcf->setWetting(values, layer);
}

void PCRModflow::setWetting(const discr::BlockData<REAL4> &values){

  if(d_bcf == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setWetting");
  }

  if(d_wetting==NULL) {
    d_wetting = new discr::BlockData<REAL4>(d_baseArea);
  }
  d_bcf->setWetting(values);
}

void PCRModflow::setStorage(const discr::BlockData<REAL4> &primary, const discr::BlockData<REAL4> &secondary) {
  if(d_primaryStorage == NULL) {
    d_primaryStorage = new discr::BlockData<REAL4>(d_baseArea);
    d_secondaryStorage = new discr::BlockData<REAL4>(d_baseArea);
  }
  d_bcf->setStorage(primary, secondary);
}

/// \todo skip setting the hcond if layer is confining bed
bool PCRModflow::setHCond(const float *values, size_t layer, size_t type){
  layer--; // layer number passed by user starts with 1
  d_gridCheck->isGrid(layer, "setConductivity");
  d_gridCheck->testMV(values, "setHorizontalConductivity");
  bool result = setBlockData(*d_hCond, values, layer);
  if(result == true) {
    d_layerType.push_back(type);
  }
  return result;
}

void PCRModflow::setHCond(const discr::BlockData<REAL4> &values, const discr::BlockData<INT4> &type) {
  d_bcf->setHCond(values, type);
}

void PCRModflow::setCond(size_t laycon, const calc::Field *hcond, const calc::Field *vcond, size_t layer){
  d_bcf->setCond(laycon, hcond, vcond, layer);
}


bool PCRModflow::setVCond(const float *values, size_t layer) {
  layer--; // layer number passed by user starts with 1
  d_gridCheck->testMV(values, "setVerticalConductivity");
  d_gridCheck->setVCond(layer, "setVerticalConductivity");
  bool result = setBlockData(*d_vCond, values, layer);
  return result;
}

void PCRModflow::setVCond(const discr::BlockData<REAL4> &values) {
  d_bcf->setVCond(values);
}



void PCRModflow::initRCH(size_t option){
  d_rch = new RCH(this, option);

  initREAL4BlockData(&d_recharge, 0.0);
 // initREAL4BlockData(&d_rechargeResult, 0.0);
}

void PCRModflow::setRecharge(const float *values, size_t optCode) {
  if(! ((optCode == 1) || (optCode == 3)) ) {
    std::string stmp("Input error: set recharge option code either to 1 or 3 or use setIndicatedRecharge");
    d_cmethods->error(stmp, "setRecharge");
  }
  if(d_rch==NULL) {
    initRCH(optCode);
  }
  setBlockData(*d_recharge, values, 0);
}


void PCRModflow::setRechargeLay(const float *rch, const int *layer) {
  if(d_rch == NULL) {
    initRCH(2);
  }
  if(d_rechargeIrch == NULL) {
    d_rechargeIrch = new discr::BlockData<INT4>(d_baseArea);
  }
  /* bool result = */ setBlockData(*d_rechargeIrch, layer, 0);
  /* result =  */ setBlockData(*d_recharge, rch, 0);
}


void PCRModflow::setRecharge(const calc::Field *rch, size_t optCode) {
  if(d_rch == NULL) {
    initRCH(optCode);
  }
  d_rch->setRecharge(rch, optCode);
}


void PCRModflow::setRechargeLay(const calc::Field *rch, const calc::Field *layer) {
  if(d_rch == NULL) {
    initRCH(2);
  }
  d_rch->setIndicatedRecharge(rch, layer);
}


void PCRModflow::removeTextFiles(std::string const & fileName) const
{
  if(boost::filesystem::exists(fileName)) {
    boost::filesystem::remove(fileName);
  }
}

bool PCRModflow::runModflow(const std::string & working_directory) {

  modflow_directory = working_directory;

  if (d_modflow_converged == false) {
    std::string stmp("The previous execution of Modflow failed to converge");
    d_cmethods->error(stmp, "run");
    exit(1);
  }

  if(d_dis == NULL) {
    std::string stmp("Can not execute Modflow: No grid specified. Use createBottomLayer and addLayer");
    d_cmethods->error(stmp, "run");
  }
  if(d_bas == NULL) {
    std::string stmp("Can not execute Modflow: No BAS package specified");
    d_cmethods->error(stmp, "run");
  }

  if(d_bcf == NULL) {
    std::string stmp("Can not execute Modflow: No BCF package specified");
    d_cmethods->error(stmp, "run");
  }

  // at least one solver package must be specified
  if (d_solver_used == NO_SOLVER) {
    std::string stmp("Can not execute Modflow: No solver package specified");
    d_cmethods->error(stmp, "run");
  }


  // remove previous MF ouput files
  // old listing file
  removeTextFiles(mf::execution_path(run_directory(), "pcrmf.lst"));



  // do tests only if grid specification has changed since the
  // last MF run
  if(d_gridIsFixed == false) {
    // test if top layer is a Q3dConfined layer
    size_t size = d_quasiConfined.size();
    if(d_quasiConfined.at(size-2) == 1) {
      std::string stmp("Grid definition: layer 1 is a confined layer");
      d_cmethods->error(stmp, "run");
    }
    // tickness of layer must be greater than 0
    d_gridCheck->testElevation();
  }
  bool result = true;

  // the following files do not change without grid modification
  // just write them once
  if(d_gridIsFixed == false) {
    //result = writeNAM();

    //result = writeOC();

    //result = d_dis->writeDIS();

    d_dis->write_dis(run_directory());
    d_dis->write_dis_array(run_directory());

    //d_bcf->writeBCF();

    d_bcf->write(run_directory());
    d_bcf->write_hy(run_directory());
    d_bcf->write_tran(run_directory());
    d_bcf->write_vcond(run_directory());

    if(d_bcf->transient()){
      d_bcf->write_sf1(run_directory());
      d_bcf->write_sf2(run_directory());
    }
    if(d_bcf->rewetting()){
     d_bcf->write_wetdry(run_directory());
    }

  }

  //d_bas->writeBAS();
  d_bas->write(run_directory());
  d_bas->write_head_array(run_directory());
  d_bas->write_bound_array(run_directory());

///==============================================
  if(d_pcg != NULL) {
    if(d_pcg->modified()){
      std::stringstream content;
      content << *d_pcg;
      d_cmethods->writeToFile(mf::execution_path(run_directory(), "pcrmf.pcg"),content.str());
      d_pcg->update();
    }
  }
  if(d_sip != NULL) {
    if(d_sip->modified()){
      std::stringstream content;
      content << *d_sip;
      d_cmethods->writeToFile(mf::execution_path(run_directory(), "pcrmf.sip"),content.str());
      d_sip->update();
    }
  }
  if(d_sor != NULL) {
    if(d_sor->modified()){
      std::stringstream content;
      content << *d_sor;
      d_cmethods->writeToFile(mf::execution_path(run_directory(), "pcrmf.sor"),content.str());
      d_sor->update();
    }
  }
  if(d_dsp != NULL) {
    if(d_dsp->modified()){
      std::stringstream content;
      content << *d_dsp;
      d_cmethods->writeToFile(mf::execution_path(run_directory(), "pcrmf.de4"),content.str());
      d_dsp->update();
    }
  }
///==============================================
  if(d_drn != NULL) {
    if(d_drn->drainUpdated()){
      d_drn->write_list(run_directory());
      d_drn->write(run_directory());
      d_drn->setDrainUpdated(false);
    }
  }

  if(d_riv!=NULL) {
    if(d_riv->riverUpdated()){
      //d_riv ->writeRIV();
      d_riv->write_list(run_directory());
      d_riv->write(run_directory());
      d_riv->setRiverUpdated(false);
    }
  }


  if(d_rch!=NULL) {
      //d_rch->writeRCH();

    d_rch->write(run_directory());
    d_rch->write_array(run_directory());
    if(d_rch->indicated_recharge()){
      d_rch->write_indicated(run_directory());
    }
  }

  if(d_wel != NULL) {
    //d_wel->writeWEL();
    d_wel->write_list(run_directory());
    d_wel->write(run_directory());
  }

  // to make sure that wel/riv/etc packages can be activated after the first
  // time step, always write the nam file
  result = writeNAM();
  // Always write the DIS file as stress period lenghts etc can change per PCRaster time step
  d_dis->write_dis(run_directory());
  // Always write the OC file as stress period lenghts etc can change per PCRaster time step
  result = writeOC();


  if(result == true) {
    d_gridIsFixed = true;

    // old stuff...
    //int ignore = system("pcrmf2k pcrmf.nam");
    //(void)ignore; // Shut up compiler

    QProcess process;
    QString working_directory = process.workingDirectory();
    process.setWorkingDirectory(QString::fromStdString(run_directory()));

    // Standard execution
    if(run_command.empty()){
      QString program{"mf2005"};
      QStringList arg{"pcrmf.nam"};
      process.start(program, arg);
      process.waitForFinished(-1);
    }
    // User provided string
    else{
      QString program{QString::fromStdString(run_command)};
      QStringList arg{QString::fromStdString(run_arguments)};
      process.start(program, arg);
      process.waitForFinished(-1);

    }

    process.setWorkingDirectory(working_directory);

    // modflow seems to always return 0, also in error case :(
    // check listing file
    modflow_converged();
  }
  else {
    std::cerr << "Can not write MODFLOW input files" << std::endl;
    exit(1);
  }

//   if(!boost::filesystem::exists("fort.6")) {
//     std::cerr << std::endl;
//     std::cerr << "The execution of MODFLOW failed" << std::endl;
//     std::string stmp("MODFLOW terminated abnormally");
//     printList();
//     d_cmethods->error(stmp, "run");
//   }

  // get MF output
  d_bas->getHeadsFromBinary(run_directory());
  d_bas->getBASBlockData(*d_ibound, run_directory());

//  if(d_riv != NULL) {
//	   d_riv->getRivLeakFromBinary();
//  }
//  if(d_rch != NULL) {
//	   d_rch->getFlowFromBinary();
//  }
//  if(d_drn != NULL) {
//	    d_drn->getDrainFromBinary();
//  }

  return result;
}

void PCRModflow::modflow_converged() {

  std::string filename = mf::execution_path(run_directory(), "pcrmf.lst");

  if(!boost::filesystem::exists(filename)) {
    std::cerr << "  Error in PCRasterModflow: can not open global list file " << filename << std::endl;
    exit(1);
  }
  std::ifstream fileInput(filename);
  size_t offset;
  std::string line;
  //std::string search("****FAILED TO CONVERGE IN TIME STEP "); mf2000
  std::string search("TO MEET SOLVER CONVERGENCE CRITERIA");
  if (fileInput.is_open()) {
    while (!fileInput.eof()) {
      getline(fileInput, line);
      if ((offset = line.find(search, 0)) != std::string::npos) {
        d_modflow_converged = false;
        printList();
        std::cerr << std::endl;
        std::cerr << "Error: MODFLOW failed to converge" << std::endl;
      }
    }
    fileInput.close();
  }
}

void PCRModflow::printList() {

  std::string filename = mf::execution_path(run_directory(), "pcrmf.lst");

  if(!boost::filesystem::exists(filename)) {
    std::cerr << "  Error in PCRasterModflow: can not open global list file " << filename << std::endl;
    exit(1);
  }
  std::cout << "  Tail of global list file " << filename << ":" << std::endl;
  std::ifstream file;  // Datei-Handle
  std::string line;
  file.open(filename, std::ios::in);
  file.seekg(0, std::ios::end);
  size_t len = file.tellg();
  size_t last = 3000;
  if(len <= last){
    last = len - 1;
  }
  file.seekg(len - last);
  while(!file.eof()) {
    getline(file, line);
    std::cout << line << std::endl;

  }
  file.close();
}

void PCRModflow::setDISParams(size_t timeUnits, size_t lentghUnits, float stressPeriodLength, size_t nrOfTimesteps, float timeStepMultiplier, bool isSteadyState) {

  if(d_dis == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setDISParameter");
  }

  d_isSteadyState = isSteadyState;
  if(isSteadyState == false) {
    if(d_primaryStorage == NULL) {
      d_primaryStorage = new discr::BlockData<REAL4>(d_baseArea);
      d_secondaryStorage = new discr::BlockData<REAL4>(d_baseArea);
    }
  }
  if(d_gridIsFixed == true) {
    resetGrid(false);
    d_gridIsFixed = false;
  }
  d_dis->setParams(timeUnits, lentghUnits, stressPeriodLength, nrOfTimesteps, timeStepMultiplier, isSteadyState);
}

void PCRModflow::setNoFlowConstant(float value) {

  if(d_bas == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setNoFlowHead");
  }

  d_bas->setNoFlowConstant(value);
}



void PCRModflow::setTRPY(float trpy) {
  d_bcf->setTRPY(trpy);
}

void PCRModflow::setHDRY(float hdry) {
  d_bcf->setHDRY(hdry);
}

//
// BCF
//
calc::Field* PCRModflow::get_storage(size_t layer){
  return d_bcf->get_storage(layer, run_directory());
}

void PCRModflow::get_storage(float *values, size_t layer) {
  d_bcf->get_storage(values, layer, run_directory());
}


calc::Field* PCRModflow::get_constand_head(size_t layer){
  return d_bcf->get_constand_head(layer, run_directory());
}

void PCRModflow::get_constand_head(float *values, size_t layer) {
  d_bcf->get_constand_head(values, layer, run_directory());
}


calc::Field* PCRModflow::get_right_face(size_t layer){
  return d_bcf->get_right_face(layer, run_directory());
}

void PCRModflow::get_right_face(float *values, size_t layer) {
  d_bcf->get_right_face(values, layer, run_directory());
}


calc::Field* PCRModflow::get_front_face(size_t layer){
  return d_bcf->get_front_face(layer, run_directory());
}

void PCRModflow::get_front_face(float *values, size_t layer) {
  d_bcf->get_front_face(values, layer, run_directory());
}


calc::Field* PCRModflow::get_lower_face(size_t layer){
  return d_bcf->get_lower_face(layer, run_directory());
}

void PCRModflow::get_lower_face(float *values, size_t layer) {
  d_bcf->get_lower_face(values, layer, run_directory());
}


//
// DRN package
//
void PCRModflow::initDRN() {

  if(d_dis == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setDrain");
  }

  REAL4 init = 0.0;
  d_drn = new DRN(this);
  d_drnElev = new discr::BlockData<REAL4>(d_baseArea, init);
  d_drnCond = new discr::BlockData<REAL4>(d_baseArea, init);
  //d_drnResult = new discr::BlockData<REAL4>(d_baseArea, init);
}


bool PCRModflow::setDrain(const float *elevation, const float *conductance, size_t layer) {
  //if(d_gridIsFixed == true) {
  //  resetGrid(false);
  //  d_gridIsFixed = false;
  //}
  if(d_drn == NULL) {
    initDRN();
  }
  layer--; // layer number passed by user starts with 1
  d_gridCheck->isGrid(layer, "setDrain");
  d_gridCheck->isConfined(layer, "setDrain");
  d_gridCheck->testMV(elevation, "setDrain elevation");
  d_gridCheck->testMV(conductance, "setDrain conductance");
  return d_drn->setDrain(elevation, conductance, layer);
}

void PCRModflow::setDrain(const calc::Field *elevation, const calc::Field *conductance, size_t layer){
  //if(d_gridIsFixed == true) {
  // resetGrid(false);
  //  d_gridIsFixed = false;
  //}
  if(d_drn == NULL) {
    initDRN();
  }
  d_drn->setDrain(elevation, conductance, layer);
}


void PCRModflow::setDrain(const discr::BlockData<REAL4> &elevation, const discr::BlockData<REAL4> &conductance) {
  //if(d_gridIsFixed == true) {
  //  resetGrid(false);
  //  d_gridIsFixed = false;
  //}
  if(d_drn == NULL) {
    initDRN();
  }
  d_drn->setDrain(elevation, conductance);
}


/**
 *
 */
void PCRModflow::getDrain(float *values, size_t layer) {
  if(d_drn == NULL) {
    std::string stmp("No drain values specified: Define elevation and conductance values");
    d_cmethods->error(stmp, "getDrain");
  }
  layer--; // layer number passed by user starts with 1
  d_gridCheck->isGrid(layer, "getDrain");
  d_gridCheck->isConfined(layer, "getDrain");
  d_drn->getDrain(values, layer, run_directory());
}

calc::Field* PCRModflow::getDrain(size_t layer){
  if(NULL == d_drn) {
    std::string stmp("No drain values specified: Define elevation and conductance values");
    d_cmethods->error(stmp, "getDrain");
  }
  return d_drn->getDrain(layer, run_directory());
}

//
// WEL package
//
void PCRModflow::initWEL() {

  if(d_dis == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setWell");
  }

  d_wel = new WEL(this);
  d_welValues = new discr::BlockData<REAL4>(d_baseArea, 0.0);
}

bool PCRModflow::setWell(const float *well, size_t layer) {
  if(d_wel == NULL) {
    initWEL();
  }
  layer--; // layer number passed by user starts with 1
  d_gridCheck->isGrid(layer, "setWell");
  d_gridCheck->isConfined(layer, "setWell");
  return d_wel->setWell(well, layer);
}

void PCRModflow::setWell(const calc::Field *well, size_t layer){
  if(d_wel == NULL) {
    initWEL();
  }
  d_wel->setWell(well, layer);
}

void PCRModflow::setWell(discr::BlockData<REAL4> &well) {
  if(d_wel == NULL) {
    initWEL();
  }
  d_wel->setWell(well);
}


//
// Solver packages
//
void PCRModflow::setSOR(size_t mxiter, double accl, double hclose) {

  if(d_solver_used != NO_SOLVER && d_solver_used != SOR_SOLVER){
    std::string stmp("A solver package different to SOR was previously specified");
    d_cmethods->error(stmp, "setSOR");
  }

  if(d_solver_used == NO_SOLVER){
    d_solver_used = SOR_SOLVER;
    d_sor = new SOR();
  }

  d_sor->setSOR(mxiter, accl, hclose, true);
  d_modflow_converged = true;
}


void PCRModflow::setSIP(size_t mxiter, size_t nparam, double accl, double hclose, size_t ipcalc, double wseed) {

  if(d_solver_used != NO_SOLVER && d_solver_used != SIP_SOLVER){
    std::string stmp("A solver package different to SIP was previously specified");
    d_cmethods->error(stmp, "setSIP");
  }

  if(d_solver_used == NO_SOLVER){
    d_solver_used = SIP_SOLVER;
    d_sip = new SIP();
  }

  d_sip->setSIP(mxiter, nparam, accl, hclose, ipcalc, wseed, true);
  d_modflow_converged = true;
}


void PCRModflow::setPCG(size_t mxiter, size_t iteri,  size_t npcond, double hclose, double rclose, double relax,  double nbpol, double damp) {

  if(d_solver_used != NO_SOLVER && d_solver_used != PCG_SOLVER){
    std::string stmp("A solver package different to PCG was previously specified");
    d_cmethods->error(stmp, "setPCG");
  }

  if(d_solver_used == NO_SOLVER){
    d_solver_used = PCG_SOLVER;
    d_pcg = new PCG();
  }

  d_pcg->setPCG(mxiter, iteri, npcond, hclose, rclose, relax, nbpol, damp, true);
  d_modflow_converged = true;
}


void PCRModflow::setDSP(size_t itmx, size_t mxup, size_t mxlow, size_t mxbw, size_t ifreq, double accl, double hclose) {

  if(d_solver_used != NO_SOLVER && d_solver_used != DSP_SOLVER){
    std::string stmp("A solver package different to DSP was previously specified");
    d_cmethods->error(stmp, "setDSP");
  }

  if(d_solver_used == NO_SOLVER){
    d_solver_used = DSP_SOLVER;
    d_dsp = new DSP();
  }

  d_dsp->setDSP(itmx, mxup, mxlow, mxbw, ifreq, accl, hclose, true);
  d_modflow_converged = true;
}




void PCRModflow::createBottomPS(const std::string & lower, const std::string & upper){
  dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster1(rasterdal.read(lower, dal::TI_REAL4));
  boost::shared_ptr<dal::Raster> raster2(rasterdal.read(upper, dal::TI_REAL4));
  createBottom(static_cast<REAL4 const*>(raster1->cells()), static_cast<REAL4 const*>(raster2->cells()));
}

void PCRModflow::addLayerPS(const std::string & values){
  dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster(rasterdal.read(values, dal::TI_REAL4));
  addLayer(static_cast<REAL4 const*>(raster->cells()));
}

void PCRModflow::addConfinedLayerPS(const std::string & values){
  dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster(rasterdal.read(values, dal::TI_REAL4));
  addConfinedLayer(static_cast<REAL4 const*>(raster->cells()));
}


void PCRModflow::setIBound(const std::string & values, size_t layer){
  dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster(rasterdal.read(values, dal::TI_INT4));
  setIBound(static_cast<INT4 const*>(raster->cells()),layer);
}



  void PCRModflow::setInitialHead(const std::string & values, size_t layer){
   dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster(rasterdal.read(values, dal::TI_REAL4));
  setInitialHead(static_cast<REAL4 const*>(raster->cells()), layer);
}



  void PCRModflow::setCond(size_t laycon, const std::string & hcond, const std::string & vcond, size_t layer){
   dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster1(rasterdal.read(hcond, dal::TI_REAL4));
  boost::shared_ptr<dal::Raster> raster2(rasterdal.read(vcond, dal::TI_REAL4));

  setHCond(static_cast<REAL4 const*>(raster1->cells()),  layer, laycon);
  setVCond(static_cast<REAL4 const*>(raster2->cells()),  layer);
}

  void PCRModflow::setRecharge(const std::string & values, size_t optCode){
     dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster(rasterdal.read(values, dal::TI_REAL4));
  setRecharge(static_cast<REAL4 const*>(raster->cells()), optCode);
}

  void PCRModflow::setRechargeLay(const std::string & values, const std::string & layer){
     dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster1(rasterdal.read(values, dal::TI_REAL4));
  boost::shared_ptr<dal::Raster> raster2(rasterdal.read(layer, dal::TI_INT4));
  setRechargeLay(static_cast<REAL4 const*>(raster1->cells()), static_cast<INT4 const*>(raster2->cells()));
}


  void PCRModflow::setWetting(const std::string & values, size_t mfLayer){

  if(d_bcf == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setWetting");
  }

     dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster(rasterdal.read(values, dal::TI_REAL4));
  setWetting(static_cast<REAL4 const*>(raster->cells()), mfLayer);
}


  void PCRModflow::setWell(const std::string & values, size_t mfLayer){
     dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster(rasterdal.read(values, dal::TI_REAL4));
  setWell(static_cast<REAL4 const*>(raster->cells()), mfLayer);
}




 void PCRModflow::setStorage(const std::string & prim, const std::string & second, size_t layer){

  if(d_bcf == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setStorage");
  }
    dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster1(rasterdal.read(prim, dal::TI_REAL4));
  boost::shared_ptr<dal::Raster> raster2(rasterdal.read(second, dal::TI_REAL4));

  setPrimaryStorage(static_cast<REAL4 const*>(raster1->cells()), layer);
  setSecondaryStorage(static_cast<REAL4 const*>(raster2->cells()), layer);
}

 void PCRModflow::setRiver(const std::string & rivH, const std::string & rivB, const std::string & rivC, size_t layer){
  dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster1(rasterdal.read(rivH, dal::TI_REAL4));
  boost::shared_ptr<dal::Raster> raster2(rasterdal.read(rivB, dal::TI_REAL4));
  boost::shared_ptr<dal::Raster> raster3(rasterdal.read(rivC, dal::TI_REAL4));

  setRiver(static_cast<REAL4 const*>(raster1->cells()), static_cast<REAL4 const*>(raster2->cells()), static_cast<REAL4 const*>(raster3->cells()), layer);
}


 void PCRModflow::setDrain(const std::string & elevation, const std::string & conductance, size_t layer){
     dal::RasterDal rasterdal(true);
  boost::shared_ptr<dal::Raster> raster1(rasterdal.read(elevation, dal::TI_REAL4));
  boost::shared_ptr<dal::Raster> raster2(rasterdal.read(conductance, dal::TI_REAL4));

  setDrain(static_cast<REAL4 const*>(raster1->cells()), static_cast<REAL4 const*>(raster2->cells()), layer);
}




size_t PCRModflow::get_modflow_layernr(size_t layer) {
  // return for a pcrmodflow layer number a modflow layer number
  size_t result = 999999;

  for(size_t pos = 0; pos < d_layer2BlockLayer.size(); ++pos){
      if(mfLayer2BlockLayer(pos) == layer){
        result = pos;
      }
  }

  return result;
}


void PCRModflow::set_row_width(boost::python::list const& arguments){

  if(d_dis == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setRowWidth");
  }

  size_t nr_args = boost::python::len(arguments);

  if(nr_args != d_nrOfRows){
    std::stringstream tmp;
    tmp << nr_args << " row widths given while " << d_nrOfRows << " are required";
    d_cmethods->error(tmp.str(), "setRowWidth");
  }

  d_dis->reset_row_width();

  for(size_t idx = 0; idx < nr_args; ++idx){
    d_dis->append_row_width(boost::python::extract<float>(arguments[idx]));
  }

}



void PCRModflow::set_col_width(boost::python::list const& arguments){

  if(d_dis == 0){
    std::string stmp("Layers need to be specified at first!");
    d_cmethods->error(stmp, "setColumnWidth");
  }

  size_t nr_args = boost::python::len(arguments);

  if(nr_args != d_nrOfColumns){
    std::stringstream tmp;
    tmp << nr_args << " column widths given while " << d_nrOfColumns << " are required";
    d_cmethods->error(tmp.str(), "setColumnWidth");
  }

  d_dis->reset_col_width();

  for(size_t idx = 0; idx < nr_args; ++idx){
    d_dis->append_col_width(boost::python::extract<float>(arguments[idx]));
  }

}

bool PCRModflow::converged(){
  return d_modflow_converged;
}

std::string PCRModflow::run_directory(){
  return modflow_directory;
}


void PCRModflow::update_dis_parameter(float stressPeriodLength, size_t nrOfTimesteps, float timeStepMultiplier){
  d_dis->update_parameter(stressPeriodLength, nrOfTimesteps, timeStepMultiplier);
}

void PCRModflow::set_run_command(const std::string & command, const std::string & arguments){
  run_command = command;
  run_arguments = arguments;
}

