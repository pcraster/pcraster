#ifndef INCLUDED_RCH
#include "rch.h"
#define INCLUDED_RCH
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

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif


// PCRaster library headers.
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif


// Module headers.
#ifndef INCLUDED_COMMON
#include "common.h"
#define INCLUDED_COMMON
#endif

#ifndef INCLUDED_GRIDCHECK
#include "gridcheck.h"
#define INCLUDED_GRIDCHECK
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
RCH::~RCH(){
}

/**
 * Constructor
 */
RCH::RCH(PCRModflow *mf, size_t rchOpCode) :
  d_mf(mf),
  d_nrchop(rchOpCode),
 // d_irchcb(160),
  d_inrech(1),
  d_inirch(1),
  //d_fortran_unit_number(260),
  d_output_unit_number(260),
  d_array_unit_number(261),
  d_indicated_unit_number(262)
{
}

/**
 * write RCH to file
 */
// void RCH::writeRCH() const{
//
//   std::stringstream content;
//   content << " " << std::setw(9) << d_nrchop;
//   content << " " << std::setw(9) << d_fortran_unit_number << std::endl;
//   content << " " << std::setw(9) << d_inrech;
//   content << " " << std::setw(9) << d_inirch << std::endl;
//
//   content << "INTERNAL  1.00000E+00  (FREE)  -1";
//
//   d_mf->d_cmethods->writeMatrix(content, "", discr::BlockData<float>(*(d_mf->d_recharge)), 0);
//   if(d_nrchop == 2){
//     if(d_mf->d_rechargeIrch == NULL){
//       std::stringstream stmp;
//       stmp << "No layer number variables IRCH specified";
//       d_mf->d_cmethods->error(stmp.str(), "run");
//     }
//     d_mf->d_cmethods->writeMatrix(content, "", discr::BlockData<int>(*(d_mf->d_rechargeIrch)), 0);
//   }
//
//   d_mf->d_cmethods->writeToFile("pcrmf.rch",content.str());
// }




/**
 * retrieving recharge cell-by-cell flow values from the binary modflow output
 * rec2 : flow values values for all layer
 */
// void RCH::getFlowFromBinary(){
//   std::ifstream file("fort.160", std::ios::in | std::ios::binary);
//   if(!file.is_open()){
//     std::stringstream stmp;
//     stmp << "Can not open file containing recharge cell-by-cell flow terms";
//     d_mf->d_cmethods->errorMessage(stmp.str(), "run");
//     exit(1);
//   }
//
//   char header[mf::recordMarkerSize];
//   int headerSizeBytes = 0;
//   file.read(header, mf::recordMarkerSize);
//   std::memcpy(&headerSizeBytes, &(header[0]), 4);
//   assert(headerSizeBytes == 36);
//
//   // read the header data inclusive
//   // header information is already known, dummy read
//   char *headerData = new char[headerSizeBytes + 4];
//   file.read(headerData, headerSizeBytes); // + 4
//
//   // tail of header
//   char tailHeader[mf::recordMarkerSize + 4];
//   int tailSizeBytes = 0;
//   file.read(tailHeader, mf::recordMarkerSize);
//   std::memcpy(&tailSizeBytes, &(tailHeader[0]), 4);
//   assert(tailSizeBytes == 36);
//
//   // data header
//   char dataHeader[mf::recordMarkerSize + 4];
//   int dataSizeBytes = 0;
//   file.read(dataHeader, mf::recordMarkerSize);
//   std::memcpy(&dataSizeBytes, &(dataHeader[0]), 4);
//   // read the data
//   char *charData = new char[dataSizeBytes];
//   file.read(charData, dataSizeBytes);
//   float *floatData = reinterpret_cast<float *>(charData);
//
//   const size_t cellMax = d_mf->d_nrOfCells;
//   size_t pos = 0;
//   for(size_t layer = 0; layer < d_mf->d_nrMFLayer; layer++){
//     size_t blockLayer = d_mf->mfLayer2BlockLayer(layer);
//     for(size_t i = 0; i < cellMax; i++){
//       d_mf->d_rechargeResult->cell(i)[blockLayer] = floatData[pos];
//       pos++;
//     }
//   }
//   file.close();
//   delete[] charData;
//   charData = NULL;
//   delete[] headerData;
//   headerData = NULL;
// }


void RCH::setRecharge(const calc::Field *rch, size_t optCode){
  if(!((optCode == 1) || (optCode == 3))){
    std::string stmp("Input error: set recharge option code within either to 1 or 3");
    d_mf->d_cmethods->error(stmp, "setRecharge");
  }
  REAL8 value = 0.0;
  for(size_t i = 0; i < d_mf->d_nrOfCells; i++){
    rch->getCell(value, i);
    d_mf->d_recharge->cell(i)[0] = static_cast<REAL4>(value);
  }
}


void RCH::setIndicatedRecharge(const calc::Field *rch, const calc::Field *layer){
  if(d_mf->d_rechargeIrch == NULL){
    d_mf->d_rechargeIrch = new discr::BlockData<INT4>(d_mf->d_baseArea);
  }
  // recharge
  for(size_t i = 0; i < d_mf->d_nrOfCells; i++){
    double value = 0.0;
    rch->getCell(value, i);
    d_mf->d_recharge->cell(i)[0] = static_cast<float>(value);
  }
  // rch indicator
  for(size_t i = 0; i < d_mf->d_nrOfCells; i++){
    double value = 0.0;
    layer->getCell(value, i);
    d_mf->d_rechargeIrch->cell(i)[0] = static_cast<int>(value);
  }
}


// discr::BlockData<REAL4>* RCH::getBlockCellByCellFlow(){
//   discr::BlockData<REAL4> *resultRch = new discr::BlockData<REAL4>(d_mf->d_baseArea);
//   d_mf->d_cmethods->setDiscrBlockData(*(d_mf->d_rechargeResult), *resultRch);
//   return resultRch;
// }

calc::Field* RCH::getRecharge(size_t layer, std::string const& path) const {
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "getRiverLeakage");
  d_mf->d_gridCheck->isConfined(layer, "getRiverLeakage");

  const std::string desc("        RECHARGE");
  std::stringstream stmp;
  stmp << "Can not open file containing DRAINS cell-by-cell flow terms";

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  calc::Spatial* spatial = new calc::Spatial(VS_S, calc::CRI_f, d_mf->d_nrOfCells);
  REAL4* cells = static_cast<REAL4*>(spatial->dest());

  mf::BinaryReader reader;
  const std::string filename(mf::execution_path(path, "fort." + boost::lexical_cast<std::string>(d_output_unit_number)));
  reader.read(stmp.str(), filename, cells, desc, pos_multiplier);

  return spatial;
}


/**
 * writing rch cbc flow to PCR map
 */
void RCH::getRecharge(float *values, size_t layer, std::string const& path) const {
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "getRecharge");
  d_mf->d_gridCheck->isConfined(layer, "getRecharge");

  const std::string desc("        RECHARGE");
  std::stringstream stmp;
  stmp << "Can not open file containing recharge cell-by-cell flow terms";

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);


  //get_binary(cells, desc, start_pos, pos_multiplier);
  mf::BinaryReader reader;
  const std::string filename(mf::execution_path(path, "fort." + boost::lexical_cast<std::string>(d_output_unit_number)));
  reader.read(stmp.str(), filename, values, desc, pos_multiplier);
}



void RCH::write(std::string const& path){

  std::string filename = mf::execution_path(path, "pcrmf.rch");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  content << "# Generated by PCRaster Modflow\n";
  content << d_nrchop;
  content << " " <<  d_output_unit_number << "\n";
  content << d_inrech << " " <<  d_inirch << "\n";

  content << "EXTERNAL " << d_array_unit_number << " 1.0 (FREE) -1\n";

  if(indicated_recharge()){
    if(d_mf->d_rechargeIrch == NULL){
      std::stringstream stmp;
      stmp << "No layer number variables IRCH specified";
      d_mf->d_cmethods->error(stmp.str(), "run");
    }
    content << "EXTERNAL " << d_indicated_unit_number << " 1.0 (FREE) -1\n";
  }

  content.close();
}

void RCH::write_array(std::string const& path){

  std::string filename = mf::execution_path(path, "pcrmf_rch.asc");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  size_t count = 0;

  for(size_t r = 0; r < d_mf->d_nrOfRows; ++r){
    for(size_t c = 0; c < d_mf->d_nrOfColumns ; ++c){
      content << d_mf->d_recharge->cell(count)[0] << " ";
      count++;
    }
    content << "\n";
  }

  content.close();
}


void RCH::write_indicated(std::string const& path){

  std::string filename = mf::execution_path(path, "pcrmf_irch.asc");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  size_t count = 0;

  for(size_t r = 0; r < d_mf->d_nrOfRows; ++r){
    for(size_t c = 0; c < d_mf->d_nrOfColumns ; ++c){
      content << d_mf->d_rechargeIrch->cell(count)[0] << " ";
      count++;
    }
    content << "\n";
  }

  content.close();
}


bool RCH::indicated_recharge() const {
  return d_nrchop == 2 ? true : false;
}
