#ifndef INCLUDED_RIV
#include "riv.h"
#define INCLUDED_RIV
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
RIV::~RIV(){
}

/**
 * Constructor
 */
RIV::RIV(PCRModflow *mf) :
  d_mf(mf),
  d_riverUpdated(false),
  d_nr_river_cells(0),
  d_output_unit_number(250),
  d_input_unit_number(251){//,
  //d_fortran_unit_number(250) {
}


bool RIV::riverUpdated() const
{
  return d_riverUpdated;
}


void RIV::setRiverUpdated(bool value)
{
  d_riverUpdated = value;
}



/**
 * writes RIV to file
 */
// bool RIV::writeRIV() const{
//   std::stringstream content;
//   //float val = -1.0;
//   int count = 0;
//   int mfLayer = 1;
//   int mfCount = 0;
//   std::stringstream rivStr;
//   for(size_t layer = 1; layer<= d_mf->d_nrMFLayer; layer++){
//     count = 0;
//     size_t size = d_mf->d_layer2BlockLayer.size();
//     int blockLayer = d_mf->d_layer2BlockLayer.at(size - layer);
//
//     for(size_t row = 0; row < d_mf->d_nrOfRows; row++){
//       for(size_t col = 0; col < d_mf->d_nrOfColumns; col++){
//         float cond = d_mf->d_rivCond->cell(count)[blockLayer];
//         if(cond > 0.0){
//           rivStr << std::setw(10) << mfLayer;
//           rivStr << std::setw(10) << (row + 1);
//           rivStr << std::setw(10) << (col + 1);
//           rivStr << " " << std::setw(10) << d_mf->d_rivStage->cell(count)[blockLayer];
//           rivStr << " " << std::setw(10) << cond;
//           rivStr << " " << std::setw(10) << d_mf->d_rivBottom->cell(count)[blockLayer];
//           rivStr << std::endl;
//           mfCount++;
//         }
//         count++;
//       }
//     }
//     mfLayer++;
//   }
//   content << std::setw(10) << mfCount;
//   content << std::setw(10) << d_fortran_unit_number;
//   content << std::setw(10) << "NOPRINT" << std::endl;
//   content << std::setw(10) << mfCount;
//   content << std::setw(10) << 0 << std::endl;
//   content << rivStr.str();
//   return d_mf->d_cmethods->writeToFile("pcrmf.riv",content.str());
// }


bool RIV::setRiver(const float *rivH, const float *rivB, const float *rivC, size_t layer)
{
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "setRiver");
  d_mf->d_gridCheck->isConfined(layer, "setRiver");
  d_mf->d_methodName = "setRiver head values";
  //size_t layer = mfLayer2BlockLayer(mfLayer);
  bool result = d_mf->setBlockData(*(d_mf->d_rivStage), rivH, layer);
  d_mf->d_methodName = "setRiver bottom values";
  result = d_mf->setBlockData(*(d_mf->d_rivBottom), rivB, layer);
  d_mf->d_methodName = "setRiver conductance values";
  result = d_mf->setBlockData(*(d_mf->d_rivCond), rivC, layer);
  d_riverUpdated = true;
  return result;
}


void RIV::setRiver(const calc::Field *rivH, const calc::Field *rivB, const calc::Field *rivC, size_t layer){
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "setRiver");
  d_mf->d_gridCheck->isConfined(layer, "setRiver");
  d_mf->d_methodName = "setRiver head values";
  d_mf->setBlockData(*(d_mf->d_rivStage), rivH->src_f(), layer);
  d_mf->d_methodName = "setRiver bottom values";
  d_mf->setBlockData(*(d_mf->d_rivBottom), rivB->src_f(), layer);
  d_mf->d_methodName = "setRiver conductance values";
  d_mf->setBlockData(*(d_mf->d_rivCond), rivC->src_f(), layer);
  d_riverUpdated = true;
}



void RIV::setRiver(const discr::BlockData<REAL4> &stage, const discr::BlockData<REAL4> &bottom, const discr::BlockData<REAL4> &cond){
  d_mf->d_cmethods->setDiscrBlockData(stage, *(d_mf->d_rivStage));
  d_mf->d_cmethods->setDiscrBlockData(bottom,*(d_mf->d_rivBottom));
  d_mf->d_cmethods->setDiscrBlockData(cond, *(d_mf->d_rivCond));
  d_riverUpdated = true;
}

// discr::BlockData<REAL4>* RIV::getBlockCellByCellFlow(){
//   discr::BlockData<REAL4> *resultRiv = new discr::BlockData<REAL4>(d_mf->d_baseArea);
//   d_mf->d_cmethods->setDiscrBlockData(*(d_mf->d_rivLeakage), *resultRiv);
//   return resultRiv;
// }


/**
 *
 */
calc::Field* RIV::getRiverLeakage(size_t layer, std::string const& path) const {
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "getRiverLeakage");
  d_mf->d_gridCheck->isConfined(layer, "getRiverLeakage");

  const std::string desc("   RIVER LEAKAGE");
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
* writing river output to PCR map
*/
void RIV::getRiverLeakage(float *values, size_t layer, std::string const& path) const {
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "getRiverLeakage");
  d_mf->d_gridCheck->isConfined(layer, "getRiverLeakage");

  const std::string desc("   RIVER LEAKAGE");
  std::stringstream stmp;
  stmp << "Can not open file containing DRAINS cell-by-cell flow terms";

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  //get_binary(cells, desc, start_pos, pos_multiplier);
  mf::BinaryReader reader;
  const std::string filename(mf::execution_path(path, "fort." + boost::lexical_cast<std::string>(d_output_unit_number)));
  reader.read(stmp.str(), filename, values, desc, pos_multiplier);
}



void RIV::write(std::string const& path){

  // # riv cells is calculated by write_list
  assert(d_nr_river_cells != 0);

  std::string filename = mf::execution_path(path, "pcrmf.riv");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  content << "# Generated by PCRaster Modflow\n";
  content << d_nr_river_cells;
  content << " " << d_output_unit_number;
  content << " NOPRINT\n";
  content << d_nr_river_cells;
  content << " 0\n";
  content << "EXTERNAL " << d_input_unit_number << "\n";


  d_nr_river_cells = 0;
}


void RIV::write_list(std::string const& path){
  // This method also calculates the nr of river cells,
  // needs to be called before write
  std::string filename = mf::execution_path(path, "pcrmf_riv.asc");
  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  int count = 0;
  int mfLayer = 1;
  for(size_t layer = 1; layer<= d_mf->d_nrMFLayer; layer++){
    count = 0;
    size_t size = d_mf->d_layer2BlockLayer.size();
    int blockLayer = d_mf->d_layer2BlockLayer.at(size - layer);

    for(size_t row = 0; row < d_mf->d_nrOfRows; row++){
      for(size_t col = 0; col < d_mf->d_nrOfColumns; col++){
        float cond = d_mf->d_rivCond->cell(count)[blockLayer];
        if(cond > 0.0){
          content << mfLayer;
          content << " " << (row + 1);
          content << " " << (col + 1);
          content << " " << d_mf->d_rivStage->cell(count)[blockLayer];
          content << " " << cond;
          content << " " << d_mf->d_rivBottom->cell(count)[blockLayer];
          content << "\n";
          d_nr_river_cells++;
        }
        count++;
      }
    }
    mfLayer++;
  }
}




/**
 * retrieving river leakage values from the binary modflow output
 * format of the file is:
 * rec1 : header informations (label, top layer, nrRows, ...)
 * rec2 : leakage values values for all layer
 */
// void RIV::getRivLeakFromBinary(){
//   std::ifstream file("fort.150", std::ios::in | std::ios::binary);
//   if(!file.is_open()){
//     std::stringstream stmp;
//     stmp << "Can not open file containing river cell-by-cell flow terms";
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
//   file.read(headerData, headerSizeBytes);
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
//       d_mf->d_rivLeakage->cell(i)[blockLayer] = floatData[pos];
//       pos++;
//     }
//   }
//   file.close();
//
//   delete[] charData;
//   charData = NULL;
//   delete[] headerData;
//   headerData = NULL;
// }
