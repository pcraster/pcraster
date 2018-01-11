#ifndef INCLUDED_BCF
#include "bcf.h"
#define INCLUDED_BCF
#endif


// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_IOMANIP
#include <iomanip>
#define INCLUDED_IOMANIP
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

#ifndef INCLUDED_BOOST_LEXICAL_CAST
#include <boost/lexical_cast.hpp>
#define INCLUDED_BOOST_LEXICAL_CAST
#endif

// PCRaster library headers.
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif


// Module headers.
#ifndef INCLUDED_GRIDCHECK
#include "gridcheck.h"
#define INCLUDED_GRIDCHECK
#endif

#ifndef INCLUDED_COMMON
#include "common.h"
#define INCLUDED_COMMON
#endif

#ifndef INCLUDED_PCRMODFLOW
#include "pcrmodflow.h"
#define INCLUDED_PCRMODFLOW
#endif


#ifndef INCLUDED_MF_BINARYREADER
#include "mf_BinaryReader.h"
#define INCLUDED_MF_BINARYREADER
#endif

#include "mf_utils.h"

/**
 * Destructor
 */
BCF::~BCF(){
}

/**
 * Constructor
 */
BCF::BCF(PCRModflow *mf):
  d_iwdflg(0.0),
  d_wetfct(0.0),
  d_ihdwet(0.0),
  d_trpy(1.0),
  d_iwetit(0),
  d_hdry(-999.0),
  d_output_unit_number(240),
  d_hy_unit_number(500),
  d_vcond_unit_number(501),
  d_tran_unit_number(502),
  d_sf1_unit_number(503),
  d_sf2_unit_number(504),
  d_wet_unit_number(505),
  d_mf(mf)
{
}



/**
 * write BCF to file
 */
void BCF::writeBCF(){
  std::stringstream content;
  //size_t count_old = d_mf->d_layerType.size();
  size_t count = d_mf->d_layer2BlockLayer.size();

  //
  // Item 1: IBCFCB HDRY IWDFLG WETFCT IWETIT IHDWET
  //
  content << " " << d_output_unit_number;
  content << " " << d_hdry;
  content << " " << d_iwdflg;
  content << " " << d_wetfct;
  content << " " << d_iwetit;
  content << " " << d_ihdwet << std::endl;
  //
  // NLAY layer type
  //
  for(int i = count-1; i >= 0; i--){
    size_t mfLayer = count - i;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - mfLayer);

    content <<  " " << d_mf->d_layerType.at(blockLayer);
  }


  /*
  std::vector<int>::reverse_iterator ri = d_mf->d_layerType.rbegin();
  while(ri != d_mf->d_layerType.rend()) {
    content <<  " " << *ri;
    ++ri;
  }*/


  content << std::endl;
  //
  // TRPY
  content << "CONSTANT " << d_trpy << " TRPY" << std::endl;
  // in case of wetting values must be set
  if((d_iwdflg<0.0)||(d_iwdflg>0.0)){
    std::stringstream stmp;
    if(d_mf->d_wetting==NULL){
      stmp << "Writing BCF data failed: Wetting enabled, but no layer values defined";
      d_mf->d_cmethods->error(stmp.str(), "run");
    }
  }
  //
  // for each layer
  //
  for(int i = count-1; i >= 0; i--){
    size_t mfLayer = count - i;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - mfLayer);

    // determine layer type
    size_t lcon = getLaycon(d_mf->d_layerType.at(blockLayer));
    //
    // transient simulation, primary storage
    //

    if(d_mf->d_isSteadyState==false){
      std::stringstream stmp;
      stmp << "INTERNAL  1.00000E+00  (FREE)  -1     Sf1 layer " << mfLayer;
      d_mf->d_cmethods->writeMatrix(content, stmp.str(), d_mf->d_layer2BlockLayer, *(d_mf->d_primaryStorage), blockLayer);
    }
    //
    // Transmissivity, if laycon is 0 or 2
    //
    // if((d_mf->d_layerType.at(blockLayer)==0)||(d_mf->d_layerType.at(blockLayer)==2)){
    if((lcon == 0) || (lcon == 2)){
      std::stringstream s;
      s << "INTERNAL  1.00000E+00  (FREE)  -1    TRAN layer " << mfLayer;
      calcTran(content, blockLayer, s.str());
    }
    //
    // Hydraulic conductivity, if laycon is 1 or 3
    //
    // hydraulic conductivity along rows
    // if((d_mf->d_layerType.at(blockLayer)==1)||(d_mf->d_layerType.at(blockLayer)==3)){
    if((lcon == 1) || (lcon == 3)){
      std::string s = boost::str(boost::format("INTERNAL  1.00000E+00  (FREE)  %1%    HY layer %2%") % -1 %mfLayer);
      d_mf->d_cmethods->writeMatrix(content, s, d_mf->d_layer2BlockLayer, *(d_mf->d_hCond), blockLayer);
    }
    // vertical  conductivity along rows
    // cannot be specified for the bottom layer
    //  if( (i!=0) && ((d_mf->d_layerType.at(i)==1)||(d_mf->d_layerType.at(i)==3)) ){
    if((i!=0) && (blockLayer!=0)){// not for bottom layer, check this again...
      std::string s = boost::str(boost::format("INTERNAL  1.00000E+00  (FREE)  %1%    VCONT layer %2%") % -1 %mfLayer);
      calcVCond(content, blockLayer, s);
    }

    //
    // transient simulation, secondary storage, if laycon is 2 or 3
    //
    if(d_mf->d_isSteadyState==false){
      if((lcon == 2) || (lcon == 3)){
        std::stringstream stmp;
        stmp << "INTERNAL  1.00000E+00  (FREE)  -1     Sf2 layer " << mfLayer;
        d_mf->d_cmethods->writeMatrix(content, stmp.str(), d_mf->d_layer2BlockLayer, *(d_mf->d_secondaryStorage), blockLayer);
      }
    }
    //
    // if wetting enabled and laycon is 1 or 3
    //
    if((d_iwdflg<0.0)||(d_iwdflg>0.0)){
      //if((d_mf->d_layerType.at(i)==1) || (d_mf->d_layerType.at(i)==3)){
      if( (lcon == 1) || (lcon == 3) ){
        std::stringstream stmp;
        stmp << "INTERNAL  1.00000E+00  (FREE)  -1     WETDRY layer " << mfLayer;
        d_mf->d_cmethods->writeMatrix(content, stmp.str(), d_mf->d_layer2BlockLayer, *(d_mf->d_wetting), blockLayer);
      }
    }
  }
  d_mf->d_cmethods->writeToFile("pcrmf.bc6_old", content.str());
}

/**
 * calculate confined storage coefficient
 */
// void BCF::calcSf1(std::stringstream &aStream, size_t layer, const std::string &msg) const{
//   size_t cols = d_mf->d_nrOfColumns;
//   size_t rowWrap = d_mf->d_nrOfColumns - 1;
//   aStream << msg << std::endl;
//
//   for(size_t j = 0; j < d_mf->d_nrOfCells; ++j){
//     // sf1 = height * storage
//     float sf1 = d_mf->d_baseArea->cell(j)[layer] * d_mf->d_primaryStorage->cell(j)[layer];
//     aStream << " " << sf1;
//     if((j % cols) == rowWrap){
//       aStream << std::endl;
//     }
//   }
// }


/**
 * calculate transmissivity
 */
void BCF::calcTran(std::stringstream &aStream, size_t layer, const std::string &msg){
  size_t cols = d_mf->d_nrOfColumns;
  size_t rowWrap = d_mf->d_nrOfColumns - 1;
  aStream << msg << std::endl;

  for(size_t j = 0; j < d_mf->d_nrOfCells; ++j){
    // tran = height * hcond
    float tran = d_mf->d_baseArea->cell(j)[layer] * d_mf->d_hCond->cell(j)[layer];
    aStream << " " << tran;
    if((j % cols) == rowWrap){
      aStream << std::endl;
    }
  }
}

/**
 * test if layer has a confining bed below
 */
bool BCF::hasConfinedSubLayer(size_t layer){
 size_t lay = 0;
 int size = d_mf->d_quasiConfined.size()-1;
 if(size<1)
   return false;
 for(int i=0; i<size;i++){
   int low = d_mf->d_quasiConfined.at(i);
   int top = d_mf->d_quasiConfined.at(i + 1);
   if( ((low-top) == 1) && (lay == (layer-1))){
     return true;
   }
   lay++;
 }
 return false;
}

/**
 * calculate vertical conductivity
 */
void BCF::calcVCond(std::stringstream &aStream, size_t layer, const std::string &msg){
//   size_t cols = d_mf->d_nrOfColumns;
//   size_t rowWrap = d_mf->d_nrOfColumns - 1;
//   aStream << msg << std::endl;
//
//   for(size_t i = 0; i < d_mf->d_nrOfCells; ++i){
//     aStream  << " " << d_mf->d_vCond->cell(i)[layer] ;
//     if((i % cols) == rowWrap){
//       aStream << std::endl;
//     }
//   }
  size_t cols = d_mf->d_nrOfColumns;
  size_t rowWrap = d_mf->d_nrOfColumns - 1;
  aStream << msg << std::endl;
  bool res = hasConfinedSubLayer(layer);

  // if confining bed below
  if(res==true){

    for(size_t i = 0; i < d_mf->d_nrOfCells; ++i){
      float thickUpper = 0.5f * d_mf->d_baseArea->cell(i)[layer];
      float thickMid   =       d_mf->d_baseArea->cell(i)[layer-1];
      float thickLower = 0.5f * d_mf->d_baseArea->cell(i)[layer-2];

      float denominator = (thickUpper / d_mf->d_vCond->cell(i)[layer])
                           + (thickMid / d_mf->d_vCond->cell(i)[layer-1])
                           + (thickLower / d_mf->d_vCond->cell(i)[layer-2]);

      if(boost::math::isfinite(denominator) == 0){
        int row = 1 + i / d_mf->d_nrOfColumns;
        int col = 1 + i % d_mf->d_nrOfColumns;
        std::stringstream stmp;
        stmp << "Can not calculate VCONT in row " << row << " cell " << col << ", divsion by 0? " << std::endl;
        d_mf->d_cmethods->error(stmp.str(), "run");
      }

      float vcond = 1.0f / denominator;

      aStream << " " << vcond;
      if((i % cols) == rowWrap){
        aStream << std::endl;
      }
    }
  }
  else{
    for(size_t i = 0; i < d_mf->d_nrOfCells; i++){
      float thickUpper = 0.5f * d_mf->d_baseArea->cell(i)[layer];
      float thickLower = 0.5f * d_mf->d_baseArea->cell(i)[layer-1];

      float denominator = (thickUpper/d_mf->d_vCond->cell(i)[layer])
                           + (thickLower/d_mf->d_vCond->cell(i)[layer-1]);

      if(boost::math::isfinite(denominator) == 0){
        int row = 1 + i / d_mf->d_nrOfColumns;
        int col = 1 + i % d_mf->d_nrOfColumns;
        std::stringstream stmp;
        stmp << "Can not calculate VCONT in row " << row << " cell " << col << ", divsion by 0? " << std::endl;
        d_mf->d_cmethods->error(stmp.str(), "run");
      }

      float vcond = 1.0f / denominator;
      aStream  << " " << vcond ;
      if((i % cols) == rowWrap){
        aStream << std::endl;
      }
    }
  }
}

/**
 * setting wetting parameter
 */
void BCF::setWettingParameter(float wetfct, size_t iwetit, float ihdwet){
  d_iwdflg = 1.0;
  d_wetfct = wetfct;
  d_iwetit = iwetit;
  d_ihdwet = ihdwet;
}

/**
 * setting hor anisotropy value
 */
void BCF::setTRPY(float trpy){
  d_trpy =  trpy;
}

/**
 * setting value assigned to cells converted to dry
 */
void BCF::setHDRY(float hdry){
  d_hdry = hdry;
}


/**
 *
 */
void BCF::setHCond(const discr::BlockData<REAL4> &values, const discr::BlockData<INT4> &type){
  d_mf->d_cmethods->setDiscrBlockData(values, *(d_mf->d_hCond));
  for(size_t i = 0;  i < d_mf->d_nrMFLayer; i++){
    d_mf->d_layerType.push_back(type.cell(0)[i]);
    d_mf->dd_layerType.push_back(type.cell(0)[i]);
  }
}

/**
 *
 */
void  BCF::setVCond(const discr::BlockData<REAL4> &values){
  d_mf->d_cmethods->setDiscrBlockData(values, *(d_mf->d_vCond));
}


/**
 *
 */
void BCF::setWetting(const discr::BlockData<REAL4> &values){
  d_mf->d_cmethods->setDiscrBlockData(values, *(d_mf->d_wetting));
}

/**
 *
 */
void BCF::setStorage(const discr::BlockData<REAL4> &primary, const discr::BlockData<REAL4> &secondary){
  d_mf->d_cmethods->setDiscrBlockData(primary, *(d_mf->d_primaryStorage));
  d_mf->d_cmethods->setDiscrBlockData(secondary, *(d_mf->d_secondaryStorage));
}

/**
 *
 */
double BCF::getHDRY() const{
  return d_hdry;
}


void BCF::setCond(size_t laycon, const calc::Field *hcond, const calc::Field *vcond, size_t layer){
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "setConductivity");
  d_mf->d_gridCheck->testMV(hcond->src_f(), "setConductivity (horizontal)");

  bool result = d_mf->setBlockData(*(d_mf->d_hCond), hcond->src_f(), layer);
  if(result == true) {
    d_mf->d_layerType.push_back(laycon);
  }
  d_mf->d_gridCheck->testMV(vcond->src_f(), "setConductivity");
  d_mf->d_gridCheck->setVCond(layer, "setConductivity (vertical)");
  d_mf->setBlockData(*(d_mf->d_vCond), vcond->src_f(), layer);
}


void BCF::setStorage(const calc::Field *primary, const calc::Field *secondary, size_t layer){
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isConfined(layer, "setStorage");
  d_mf->d_gridCheck->isGrid(layer, "setStorage");
  d_mf->d_gridCheck->testMV(primary->src_f(), "setPrimaryStorage");
  d_mf->d_gridCheck->testMV(secondary->src_f(), "setSecondaryStorage");
  d_mf->setBlockData(*(d_mf->d_primaryStorage), primary->src_f(), layer);
  d_mf->setBlockData(*(d_mf->d_secondaryStorage), secondary->src_f(), layer);
}


void BCF::setWetting(const calc::Field *values, size_t layer){
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "setWetting");
  d_mf->d_gridCheck->isConfined(layer, "setWetting");
  d_mf->d_gridCheck->testMV(values->src_f(), "setWetting");
  //size_t blockLayer = mfLayer2BlockLayer(layer);
  d_mf->setBlockData(*(d_mf->d_wetting), values->src_f(), layer);
}

/*
* determine last digit of combined computing type/LAYCON flag, ie LAYCON
*/
size_t  BCF::getLaycon(size_t lcon){
  if(lcon < 10){
    return lcon;
  }
  else{
    return lcon % 10;
  }
}



void BCF::get_binary(float *values, const std::string description, size_t start, size_t multiplier, std::string const& path) const{
  // see also flow data description at faq how to read binary
  // http://water.usgs.gov/nrp/gwsoftware/modflow2000/Guide/index.html


  const std::string filename(mf::execution_path(path, "fort." + boost::lexical_cast<std::string>(d_output_unit_number)));
  //std::string filename("fort." + boost::lexical_cast<std::string>(d_output_unit_number));

  std::ifstream file(filename.c_str(), std::ios::in | std::ios::binary);
  if(!file.is_open()){
    std::stringstream stmp;
    stmp << "Can not open file containing BCF cell-by-cell flow terms";
    d_mf->d_cmethods->error(stmp.str(), "run");
  }

  size_t nr_cells = d_mf->d_nrOfCells;
  int nr_result_layer = d_mf->nr_modflow_layer();
  int nr_bytes = sizeof(float);

  // first we should check if the requested content is at 'that' position...
  // 36 metadata; 16 2 * block markers
  int skip_bytes_until_block = mf::recordMarkerSize + start * (36 + 16 + nr_cells * nr_result_layer * nr_bytes);

  file.seekg(skip_bytes_until_block);

  char tmp[4];
  file.read(tmp, 4);  // dummy read kstep
  file.read(tmp, 4);  // dummy read kper

  char* desc = new char[17];  // desc contains the array description
  file.read(desc, 16);
  desc[16] = '\0';

   if(description.compare(desc) != 0){
     std::stringstream stmp;
     stmp << "Cannot find " << description << " in the BCF output file " << filename << std::endl;
     d_mf->d_cmethods->error(stmp.str(), "run");
   }

  // jump to the right block and position, skip the metadata, block marker;
  // multiplier holds layer number of the layer we are interested in
  size_t new_pos = skip_bytes_until_block + 36 + 8 + nr_cells * multiplier * nr_bytes;
  file.seekg(new_pos);

  char *charData = new char[nr_cells * nr_bytes];
  file.read(charData, nr_cells * nr_bytes);
  float *floatData = reinterpret_cast<float *>(charData);

  for(size_t pos = 0; pos < nr_cells; ++pos){
      values[pos] = floatData[pos];
  }

  file.close();

  delete[] desc;
  desc = NULL;

  delete[] charData;
  charData = NULL;
}

/// python
calc::Field* BCF::get_storage(size_t layer, std::string const& path) const {
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "get_storage");
  d_mf->d_gridCheck->isConfined(layer, "get_storage");

  size_t start_pos = 0;    // 'first' entry in transient simulations
  const std::string desc("         STORAGE");

  if(d_mf->d_isSteadyState == true){
    std::stringstream stmp;
    stmp << "Cannot obtain storage values from a steady-state simulation";
    d_mf->d_cmethods->error(stmp.str(), "get_storage");
  }

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  calc::Spatial* spatial = new calc::Spatial(VS_S, calc::CRI_f, d_mf->d_nrOfCells);
  REAL4* cells = static_cast<REAL4*>(spatial->dest());

  get_binary(cells, desc, start_pos, pos_multiplier, path);

  return spatial;
}


/// pcrcalc
void BCF::get_storage(float *values, size_t layer, std::string const& path) const {
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "get_storage");
  d_mf->d_gridCheck->isConfined(layer, "get_storage");

  size_t start_pos = 0;    // 'first' entry in transient simulations
  const std::string desc("         STORAGE");

  if(d_mf->d_isSteadyState == true){
    std::stringstream stmp;
    stmp << "Cannot obtain storage values from a steady-state simulation";
    d_mf->d_cmethods->error(stmp.str(), "get_storage");
  }

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  get_binary(values, desc, start_pos, pos_multiplier, path);
}


/// python
calc::Field* BCF::get_constand_head(size_t layer, std::string const& path) const {
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "get_constand_head");
  d_mf->d_gridCheck->isConfined(layer, "get_constand_head");

  size_t start_pos = 1;    // 'second' entry
  const std::string desc("   CONSTANT HEAD");

  if(d_mf->d_isSteadyState == true){
     start_pos --;         // if steady-state this will be the first entry
  }

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  calc::Spatial* spatial = new calc::Spatial(VS_S, calc::CRI_f, d_mf->d_nrOfCells);
  REAL4* cells = static_cast<REAL4*>(spatial->dest());

  get_binary(cells, desc, start_pos, pos_multiplier, path);

  return spatial;
}

/// pcrcalc
void BCF::get_constand_head(float *values, size_t layer, std::string const& path) const {
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "get_constand_head");
  d_mf->d_gridCheck->isConfined(layer, "get_constand_head");

  size_t start_pos = 1;    // 'second' entry
  const std::string desc("   CONSTANT HEAD");

  if(d_mf->d_isSteadyState == true){
     start_pos --;         // if steady-state this will be the first entry
  }

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  get_binary(values, desc, start_pos, pos_multiplier, path);
}


/// python
calc::Field* BCF::get_right_face(size_t layer, std::string const& path) const {
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "get_right_face");
  d_mf->d_gridCheck->isConfined(layer, "get_right_face");

  size_t start_pos = 2;    // 'third' entry
  const std::string desc("FLOW RIGHT FACE ");

  if(d_mf->d_isSteadyState == true){
     start_pos--;
  }

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  calc::Spatial* spatial = new calc::Spatial(VS_S, calc::CRI_f, d_mf->d_nrOfCells);
  REAL4* cells = static_cast<REAL4*>(spatial->dest());

  get_binary(cells, desc, start_pos, pos_multiplier, path);

  return spatial;
}

/// pcrcalc
void BCF::get_right_face(float *values, size_t layer, std::string const& path) const {
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "get_right_face");
  d_mf->d_gridCheck->isConfined(layer, "get_right_face");

  size_t start_pos = 2;    // 'third' entry
  const std::string desc("FLOW RIGHT FACE ");

  if(d_mf->d_isSteadyState == true){
     start_pos--;
  }

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  get_binary(values, desc, start_pos, pos_multiplier, path);
}


/// python
calc::Field* BCF::get_front_face(size_t layer, std::string const& path) const {
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "get_front_face");
  d_mf->d_gridCheck->isConfined(layer, "get_front_face");

  size_t start_pos = 3;    // 'fourth' entry
  const std::string desc("FLOW FRONT FACE ");

  if(d_mf->d_isSteadyState == true){
     start_pos--;
  }

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  calc::Spatial* spatial = new calc::Spatial(VS_S, calc::CRI_f, d_mf->d_nrOfCells);
  REAL4* cells = static_cast<REAL4*>(spatial->dest());

  get_binary(cells, desc, start_pos, pos_multiplier, path);

  return spatial;
}

/// pcrcalc
void BCF::get_front_face(float *values, size_t layer, std::string const& path) const {
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "get_front_face");
  d_mf->d_gridCheck->isConfined(layer, "get_front_face");

  size_t start_pos = 3;    // 'fourth' entry
  const std::string desc("FLOW FRONT FACE ");

  if(d_mf->d_isSteadyState == true){
     start_pos--;
  }

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  get_binary(values, desc, start_pos, pos_multiplier, path);
}


/// python
calc::Field* BCF::get_lower_face(size_t layer, std::string const& path) const {
  layer--;
  if(layer == 0){
    std::stringstream stmp;
    stmp << "Cannot obtain flow lower face for bottom layer (layer "<< layer + 1 << ")";
    d_mf->d_cmethods->error(stmp.str(), "get_lower_face");
  }

  d_mf->d_gridCheck->isGrid(layer, "get_lower_face");
  d_mf->d_gridCheck->isConfined(layer, "get_lower_face");

  size_t start_pos = 4;    // 'fifth' entry
  const std::string desc("FLOW LOWER FACE ");

  if(d_mf->d_isSteadyState == true){
     start_pos--;
  }

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  calc::Spatial* spatial = new calc::Spatial(VS_S, calc::CRI_f, d_mf->d_nrOfCells);
  REAL4* cells = static_cast<REAL4*>(spatial->dest());

  get_binary(cells, desc, start_pos, pos_multiplier, path);

  return spatial;
}

/// pcrcalc
void BCF::get_lower_face(float *values, size_t layer, std::string const& path) const {
  layer--;
  if(layer == 0){
    std::stringstream stmp;
    stmp << "Cannot obtain flow lower face for bottom layer (layer "<< layer + 1 << ")";
    d_mf->d_cmethods->error(stmp.str(), "get_lower_face");
  }

  d_mf->d_gridCheck->isGrid(layer, "get_lower_face");
  d_mf->d_gridCheck->isConfined(layer, "get_lower_face");

  size_t start_pos = 4;    // 'fifth' entry
  const std::string desc("FLOW LOWER FACE ");

  if(d_mf->d_isSteadyState == true){
     start_pos--;
  }

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  get_binary(values, desc, start_pos, pos_multiplier, path);
}


bool BCF::transient() const {
  return d_mf->d_isSteadyState == false ? true : false;
}


void BCF::write(std::string const& path) {

  std::string filename = mf::execution_path(path, "pcrmf.bc6");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  // Don't add this line, Modflow does not accept comments in this package
  //content << "# Generated by PCRaster Modflow\n";

  size_t count = d_mf->d_layer2BlockLayer.size();
  //
  // Item 1: IBCFCB HDRY IWDFLG WETFCT IWETIT IHDWET
  //
  content << d_output_unit_number;
  content << " " << d_hdry;
  content << " " << d_iwdflg;
  content << " " << d_wetfct;
  content << " " << d_iwetit;
  content << " " << d_ihdwet << "\n";
  //
  // NLAY layer type
  //
  for(int i = count-1; i >= 0; i--){
    size_t mfLayer = count - i;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - mfLayer);

    content << d_mf->d_layerType.at(blockLayer) << " ";
  }

   content << "\n";

  // TRPY
  content << "CONSTANT " << d_trpy << " TRPY" << "\n";

  // in case of wetting values must be set
  if((d_iwdflg<0.0)||(d_iwdflg>0.0)){
    std::stringstream stmp;
    if(d_mf->d_wetting==NULL){
      stmp << "Writing BCF data failed: Wetting enabled, but no layer values defined";
      d_mf->d_cmethods->error(stmp.str(), "run");
    }
  }
  //
  // for each layer
  //
  for(int i = count-1; i >= 0; i--){
    size_t mfLayer = count - i;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - mfLayer);

    // determine layer type
    size_t lcon = getLaycon(d_mf->d_layerType.at(blockLayer));
    //
    // transient simulation, primary storage
    //

    if(d_mf->d_isSteadyState==false){
      content << "EXTERNAL " << d_sf1_unit_number << " 1.0 (FREE) -1 Sf1 layer " << mfLayer << "\n";
    }
    //
    // Transmissivity, if laycon is 0 or 2
    //
    if((lcon == 0) || (lcon == 2)){
      content << "EXTERNAL " << d_tran_unit_number << " 1.0 (FREE) -1 TRAN layer " << mfLayer << "\n";
    }
    //
    // Hydraulic conductivity, if laycon is 1 or 3
    //
    // hydraulic conductivity along rows
    if((lcon == 1) || (lcon == 3)){
      content << "EXTERNAL " << d_hy_unit_number << " 1.0 (FREE) -1 HY layer " << mfLayer << "\n";
    }
    // vertical  conductivity along rows
    // cannot be specified for the bottom layer
    if((i!=0) && (blockLayer!=0)){// not for bottom layer, check this again...
      content << "EXTERNAL " << d_vcond_unit_number << " 1.0 (FREE) -1 VCONT layer " << mfLayer << "\n";
    }

    //
    // transient simulation, secondary storage, if laycon is 2 or 3
    //
    if(d_mf->d_isSteadyState==false){
      if((lcon == 2) || (lcon == 3)){
        content << "EXTERNAL " << d_sf2_unit_number << " 1.0 (FREE) -1 Sf2 layer " << mfLayer << "\n";
      }
    }
    //
    // if wetting enabled and laycon is 1 or 3
    //
    if((d_iwdflg<0.0)||(d_iwdflg>0.0)){
      if( (lcon == 1) || (lcon == 3) ){
        content << "EXTERNAL " << d_wet_unit_number << " 1.0 (FREE) -1 WETDRY layer " << mfLayer << "\n";
      }
    }
  }
  content.close();
}


void BCF::write_hy(std::string const& path)  {

  std::string filename = mf::execution_path(path, "pcrmf_bcf_hy.asc");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  size_t count = d_mf->d_layer2BlockLayer.size();

  //
  // for each layer
  //
  for(int i = count-1; i >= 0; i--){
    size_t mfLayer = count - i;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - mfLayer);

    // determine layer type
    size_t lcon = getLaycon(d_mf->d_layerType.at(blockLayer));

    if((lcon == 1) || (lcon == 3)){
      size_t count = 0;

      for(size_t r = 0; r < d_mf->d_nrOfRows; ++r){
        for(size_t c = 0; c < d_mf->d_nrOfColumns ; ++c){
          content << d_mf->d_hCond->cell(count)[blockLayer] << " ";
          count++;
        }
        content << "\n";
      }
    }
  }
  content.close();
}


void BCF::write_tran(std::string const& path)  {

  std::string filename = mf::execution_path(path, "pcrmf_bcf_tran.asc");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  size_t count = d_mf->d_layer2BlockLayer.size();

  //
  // for each layer
  //
  for(int i = count-1; i >= 0; i--){
    size_t mfLayer = count - i;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - mfLayer);

    // determine layer type
    size_t lcon = getLaycon(d_mf->d_layerType.at(blockLayer));

    if((lcon == 0) || (lcon == 2)){
      size_t cols = d_mf->d_nrOfColumns;
      size_t rowWrap = d_mf->d_nrOfColumns - 1;
      //aStream << msg << std::endl;

      for(size_t j = 0; j < d_mf->d_nrOfCells; ++j){
        // tran = height * hcond
        float tran = d_mf->d_baseArea->cell(j)[blockLayer] * d_mf->d_hCond->cell(j)[blockLayer];
        content << " " << tran;
        if((j % cols) == rowWrap){
          content << "\n";
        }
      }
    }
  }
  content.close();
}


void BCF::write_vcond(std::string const& path)  {

  std::string filename = mf::execution_path(path, "pcrmf_bcf_vcont.asc");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  size_t count = d_mf->d_layer2BlockLayer.size();

  //
  // for each layer
  //
  for(int i = count-1; i >= 0; i--){
    size_t mfLayer = count - i;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - mfLayer);


    if((i!=0) && (blockLayer!=0)){
      // vertical  conductivity along rows
      size_t cols = d_mf->d_nrOfColumns;
      size_t rowWrap = d_mf->d_nrOfColumns - 1;
      bool res = hasConfinedSubLayer(blockLayer);

      // if confining bed below
      if(res==true){

        for(size_t i = 0; i < d_mf->d_nrOfCells; ++i){
          float thickUpper = 0.5f * d_mf->d_baseArea->cell(i)[blockLayer];
          float thickMid   =       d_mf->d_baseArea->cell(i)[blockLayer-1];
          float thickLower = 0.5f * d_mf->d_baseArea->cell(i)[blockLayer-2];

          float denominator = (thickUpper / d_mf->d_vCond->cell(i)[blockLayer])
                              + (thickMid / d_mf->d_vCond->cell(i)[blockLayer-1])
                              + (thickLower / d_mf->d_vCond->cell(i)[blockLayer-2]);

          if(boost::math::isfinite(denominator) == 0){
            int row = 1 + i / d_mf->d_nrOfColumns;
            int col = 1 + i % d_mf->d_nrOfColumns;
            std::stringstream stmp;
            stmp << "Can not calculate VCONT in row " << row << " cell " << col << ", divsion by 0? " << std::endl;
            d_mf->d_cmethods->error(stmp.str(), "run");
          }

          float vcond = 1.0f / denominator;

          content << " " << vcond;
          if((i % cols) == rowWrap){
            content << std::endl;
          }
        }
      }
      else{
        for(size_t i = 0; i < d_mf->d_nrOfCells; i++){
          float thickUpper = 0.5f * d_mf->d_baseArea->cell(i)[blockLayer];
          float thickLower = 0.5f * d_mf->d_baseArea->cell(i)[blockLayer-1];

          float denominator = (thickUpper/d_mf->d_vCond->cell(i)[blockLayer])
                              + (thickLower/d_mf->d_vCond->cell(i)[blockLayer-1]);

          if(boost::math::isfinite(denominator) == 0){
            int row = 1 + i / d_mf->d_nrOfColumns;
            int col = 1 + i % d_mf->d_nrOfColumns;
            std::stringstream stmp;
            stmp << "Can not calculate VCONT in row " << row << " cell " << col << ", divsion by 0? " << std::endl;
            d_mf->d_cmethods->error(stmp.str(), "run");
          }

          float vcond = 1.0f / denominator;
          content  << " " << vcond ;
          if((i % cols) == rowWrap){
            content << std::endl;
          }
        }
      }
    }
  }
  content.close();
}



bool BCF::rewetting() const {
  return d_iwdflg != 0.0;
}


void BCF::write_wetdry(std::string const& path)  {

  std::string filename = mf::execution_path(path, "pcrmf_bcf_wetdry.asc");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  size_t count = d_mf->d_layer2BlockLayer.size();

  //
  // for each layer
  //
  for(int i = count-1; i >= 0; i--){
    size_t mfLayer = count - i;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - mfLayer);

    // determine layer type
    size_t lcon = getLaycon(d_mf->d_layerType.at(blockLayer));

    if( (lcon == 1) || (lcon == 3) ){
      size_t count = 0;

      for(size_t r = 0; r < d_mf->d_nrOfRows; ++r){
        for(size_t c = 0; c < d_mf->d_nrOfColumns ; ++c){
          content << d_mf->d_wetting->cell(count)[blockLayer] << " ";
          count++;
        }
        content << "\n";
      }
    }
  }
  content.close();
}


void BCF::write_sf1(std::string const& path){

  std::string filename = mf::execution_path(path, "pcrmf_bcf_sf1.asc");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  size_t count = d_mf->d_layer2BlockLayer.size();

  for(int i = count-1; i >= 0; i--){
    size_t mfLayer = count - i;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - mfLayer);

    size_t count = 0;

    for(size_t r = 0; r < d_mf->d_nrOfRows; ++r){
      for(size_t c = 0; c < d_mf->d_nrOfColumns ; ++c){
        content << d_mf->d_primaryStorage->cell(count)[blockLayer] << " ";
        count++;
      }
      content << "\n";
    }
  }
  content.close();
}


void BCF::write_sf2(std::string const& path){

  std::string filename = mf::execution_path(path, "pcrmf_bcf_sf2.asc");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  size_t count = d_mf->d_layer2BlockLayer.size();

  for(int i = count-1; i >= 0; i--){
    size_t mfLayer = count - i;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - mfLayer);

    size_t count = 0;

    for(size_t r = 0; r < d_mf->d_nrOfRows; ++r){
      for(size_t c = 0; c < d_mf->d_nrOfColumns ; ++c){
        content << d_mf->d_secondaryStorage->cell(count)[blockLayer] << " ";
        count++;
      }
      content << "\n";
    }
  }
  content.close();
}


int BCF::hy_unit_number() const {
  return d_hy_unit_number;
}


int BCF::vcond_unit_number() const {
  return d_vcond_unit_number;
}


int BCF::wet_unit_number() const {
  return d_wet_unit_number;
}


int BCF::tran_unit_number() const {
  return d_tran_unit_number;
}


int BCF::sf1_unit_number() const {
  return d_sf1_unit_number;
}


int BCF::sf2_unit_number() const {
  return d_sf2_unit_number;
}
