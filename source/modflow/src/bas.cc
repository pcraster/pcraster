#ifndef INCLUDED_BAS
#include "bas.h"
#define INCLUDED_BAS
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#include <iomanip>

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
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

#ifndef INCLUDED_BCF
#include "bcf.h"
#define INCLUDED_BCF
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

#include <fstream>


/**
 * Destructor
 */
BAS::~BAS(){
}

/**
 * Constructor
 */
BAS::BAS(PCRModflow *amf) :
  d_mf(amf),
  d_hnoflo(-999.9),
  d_fortran_unit_number_heads(231),
  d_fortran_unit_number_bounds(232),
  d_external_unit_number_heads(400),
  d_external_unit_number_bounds(401){
}

/**
 * setting value assigned to no flow cells
 */
void BAS::setNoFlowConstant(float value){
  d_hnoflo = value;
}

/**
 * writing BAS to file
 */
// // // // // void BAS::writeBAS(){
// // // // //   std::stringstream content;
// // // // //
// // // // //   //
// // // // //   // Item 1:  options
// // // // //   //
// // // // //   content << "FREE" << std::endl;
// // // // //   //
// // // // //   // Item 2:  IBOUNDS per layer
// // // // //   //
// // // // //   size_t nrLayer = 1;
// // // // //   for(int i = d_mf->dd_nrLayer - 1; i >= 0; i--){
// // // // //
// // // // //     // only layer have boundary values
// // // // //     size_t pos = 0;
// // // // //     if(d_mf->dd_isConfined.at(i) == false){
// // // // //       content << "INTERNAL   1 (FREE)  -1  IBOUND Layer " << nrLayer << std::endl;
// // // // //       for(size_t j = 0; j < d_mf->d_nrOfRows; j++){
// // // // //         for(size_t k = 0; k < d_mf->d_nrOfColumns; k++){
// // // // //           content << d_mf->d_ibound->cell(pos)[i] << " ";
// // // // //           pos++;
// // // // //         }
// // // // //         content << std::endl;
// // // // //       }
// // // // //       nrLayer++;
// // // // //     }
// // // // //   }
// // // // //   //
// // // // //   //Item 3:  HNOFLO
// // // // //   //
// // // // //   content << "   " << d_hnoflo << "         HNOFLOW" << std::endl;
// // // // //   //
// // // // //   // Item 4:  STRT, Initial Heads per layer
// // // // //   //
// // // // //   nrLayer = 1;
// // // // //   for(int i = d_mf->dd_nrLayer - 1; i >= 0; i--){
// // // // //     std::string s = "INTERNAL 1.0 (FREE) -1 STRT";
// // // // //     d_mf->d_cmethods->writeMatrix(content, s, d_mf->d_layer2BlockLayer, *(d_mf->d_initialHead), i);
// // // // //     nrLayer++;
// // // // //   }
// // // // //   d_mf->d_cmethods->writeToFile("pcrmf.ba6_o", content.str());
// // // // // }


/**
 * retrieving heads from MF
 */
// void BAS::getBASBlockData(const std::string &filename,
//                           discr::BlockData<REAL4> &bdata){
//   std::ifstream file(filename.c_str());
//   if(!file.is_open()){
//     std::stringstream stmp;
//     stmp << "Can not open BAS result file";
//     d_mf->d_cmethods->error(stmp.str(), "run");
//   }
//   else{
//     float val;
//     int count = 0;
//     int blockLayer = 0;
//     for(size_t layer = 0; layer < d_mf->d_nrMFLayer; layer++){
//       count = 0;
//       blockLayer = d_mf->mfLayer2BlockLayer(layer);//+1);
//       // unconfinded layer retrieve values
//       for (size_t row = 0; row < d_mf->d_nrOfRows; row++){
// 	for(size_t col = 0; col < d_mf->d_nrOfColumns; col++){
// 	  file >> val;
// 	  bdata.cell(count)[blockLayer] = val;
// 	  count++;
// 	}
//       }
//     }
//     file.close();
//   }
// }

/**
 * retrieving head values from the binary modflow output
 * format of a record is:
 * record consists of
 * [record markers from gfortran 4.2 on 4 byte. still stick to 8byte
 * record marker, only this works at the moment for lin and win]
 *   - 8 byte header, first 4 byte containing the length information
 *    of the data section of the current record, second byte zero
 *   - data
 *   - 8 byte tail repeating the header information
 * format of the file is: for each layer
 * rec1 : header informations (label, top layer, nrRows, ...)
 * rec2 : head values
 * rec3 : tail
 */
void BAS::getHeadsFromBinary(std::string const& path){

  const std::string filename(mf::execution_path(path, "fort." + boost::lexical_cast<std::string>(d_fortran_unit_number_heads)));
  //std::string filename("fort." + boost::lexical_cast<std::string>(d_fortran_unit_number_heads));

  std::ifstream file(filename.c_str(), std::ios::in | std::ios::binary);
  if(!file.is_open()){
    std::stringstream stmp;
    stmp << "Can not open head value result file " << filename;
    d_mf->d_cmethods->error(stmp.str(), "run");
  }

  for(size_t layer = 0; layer < d_mf->d_nrMFLayer; layer++){
    size_t blockLayer = d_mf->mfLayer2BlockLayer(layer);//+1);
    // first record contains the header informations
    // they are omitted because we already know nrRows aso
    char header[mf::recordMarkerSize];
    int headerSizeBytes = 0;
    file.read(header, mf::recordMarkerSize);
    std::memcpy(&headerSizeBytes, &(header[0]), 4);

    assert(headerSizeBytes == 44);
    // read the header data inclusive the trailing bytes
    // header information is already known
    char *headerData = new char[headerSizeBytes + mf::recordMarkerSize];
    file.read(headerData, headerSizeBytes + mf::recordMarkerSize);
    // read the header of the data record
    char dataHeader[mf::recordMarkerSize];
    int dataSizeBytes = 0;
    file.read(dataHeader, mf::recordMarkerSize);
    std::memcpy(&dataSizeBytes, &(dataHeader[0]), 4);
    // read the data
    char *charData = new char[dataSizeBytes];
    file.read(charData, dataSizeBytes);
    REAL4 *floatData = reinterpret_cast<REAL4 *>(charData);

    size_t cellMax = d_mf->d_nrOfCells;
    //int k = 0;
    for(size_t pos = 0; pos < cellMax; pos++){
      REAL4 val = static_cast<REAL4>( floatData[pos]);
      d_mf->d_initialHead->cell(pos)[blockLayer] =  val;
    }
    // read the tailing bytes, discard content
    file.read(header, mf::recordMarkerSize);
    delete[] charData;
    charData = NULL;
    delete[] headerData;
    headerData = NULL;
  }
  file.close();
}

/**
 * retrieving bounds from MF
 */
void BAS::getBASBlockData(discr::BlockData<INT4> &bdata, std::string const& path){

  const std::string filename(mf::execution_path(path, "fort." + boost::lexical_cast<std::string>(d_fortran_unit_number_bounds)));
  //std::string filename("fort." + boost::lexical_cast<std::string>(d_fortran_unit_number_bounds));

  std::ifstream file(filename.c_str());
  if(!file.is_open()){
    std::stringstream stmp;
    stmp << "Can not open BAS result file " << filename;
    d_mf->d_cmethods->error(stmp.str(), "run");
  }
  else{
    int val;
    int count = 0;
    int blockLayer = 0;
    for(size_t layer = 0; layer < d_mf->d_nrMFLayer; layer++){
      count = 0;
      blockLayer = d_mf->mfLayer2BlockLayer(layer);
      for (size_t row = 0; row < d_mf->d_nrOfRows; row++){
	for(size_t col = 0; col < d_mf->d_nrOfColumns; col++){
	  file >> val;
	  bdata.cell(count)[blockLayer] = val;
	  count++;
	}
      }
    }
    file.close();
  }
}

/**
 * \param result result values, hdry cells are set to MV
 * \param layer layer number
 */
void BAS::getHeads(float *result, size_t layer){
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "getHeads");
  d_mf->d_gridCheck->isConfined(layer, "getHeads");

  REAL4 hdry = d_mf->d_bcf->getHDRY();
  // mf to block
  for(size_t i = 0; i < d_mf->d_nrOfCells; ++i){
    REAL4 value = d_mf->d_initialHead->cell(i)[layer];

    //if(static_cast<float>(std::fabs((value - d_hnoflo))>0.00001)){
    if(static_cast<float>(std::fabs((value - hdry)) > 0.00001)){
    //if(value-hdry==0){
      result[i] = value;
    }
    else{
      pcr::setMV(result[i]);
    }
  }
}




discr::BlockData<REAL4>* BAS::getHeads(){
  //REAL4 init = 0.0;
  discr::Raster raster(50,50,50.0,50.0,50.0);
  discr::Block bla(raster);//NULL;//d_mf->d_baseArea;
  discr::BlockData<REAL4> block(&bla);
  discr::BlockData<REAL4> *resultHeads;//
  resultHeads = new discr::BlockData<REAL4>( &bla);
  d_mf->d_cmethods->setDiscrBlockData(*(d_mf->d_initialHead), *resultHeads);
  return resultHeads;
}



calc::Field* BAS::getHeads(size_t layer){
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "getHeads");
  d_mf->d_gridCheck->isConfined(layer, "getHeads");

  calc::Spatial* spatial = new calc::Spatial(VS_S, calc::CRI_f, d_mf->d_nrOfCells);
  REAL4* cells = static_cast<REAL4*>(spatial->dest());



  float hdry = d_mf->d_bcf->getHDRY();
  for(size_t pos = 0; pos < d_mf->d_nrOfCells; pos++){
    REAL4 value = d_mf->d_initialHead->cell(pos)[layer];
    //if(std::fabs(static_cast<float>(value - d_hnoflo)) < 0.00001){
    if(std::fabs(static_cast<float>(value - hdry)) < 0.00001){
      pcr::setMV(cells[pos]);
    }
    else{
      cells[pos] = static_cast<REAL4>(value);
    }
  }
  return spatial;
}



void BAS::setBASBlockData(const discr::BlockData<INT4> &source, discr::BlockData<INT4> &result){
  d_mf->d_cmethods->setDiscrBlockData(source, result);
}

void BAS::setBASBlockData(const discr::BlockData<REAL4> &source, discr::BlockData<REAL4> &result){
  d_mf->d_cmethods->setDiscrBlockData(source, result);
}


void BAS::setIBound(const calc::Field *values, size_t layer){
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "setBoundary");
  d_mf->d_gridCheck->isConfined(layer, "setBoundary");
  d_mf->d_gridCheck->testMV(values->src_4(), "setBoundary");
  // size_t blockLayer = mfLayer2BlockLayer(mfLayer);
  d_mf->setBlockData(*(d_mf->d_ibound), values->src_4(), layer);
}


void BAS::setInitialHead(const calc::Field *values, size_t layer){
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "setInitialHead");
  d_mf->d_gridCheck->isConfined(layer, "setInitialHead");
  d_mf->d_gridCheck->testMV(values->src_f(), "setInitialHead");
  //size_t blockLayer = mfLayer2BlockLayer(mfLayer);
  d_mf->setBlockData(*(d_mf->d_initialHead), values->src_f(), layer);
}

int BAS::fortran_unit_number_heads() const {
  return d_fortran_unit_number_heads;
}

int BAS::fortran_unit_number_bounds() const {
  return d_fortran_unit_number_bounds;
}


void BAS::write(std::string const& path) const {

  std::stringstream content;

  content << "# Generated by PCRaster Modflow" << std::endl;
  //
  // Item 1:  options
  //
  content << "FREE" << std::endl;
  //
  // Item 2:  IBOUNDS per layer
  //
  size_t nrLayer = 1;
  for(int i = d_mf->dd_nrLayer - 1; i >= 0; i--){

    // only layer have boundary values
    if(d_mf->dd_isConfined.at(i) == false){
      content << "EXTERNAL " << d_external_unit_number_bounds << " 1 (FREE) -1 IBOUND Layer " << nrLayer << std::endl;
      nrLayer++;
    }
  }
  //
  //Item 3:  HNOFLO
  //
  content << "   " << d_hnoflo << "         HNOFLOW" << std::endl;
  //
  // Item 4:  STRT, Initial Heads per layer
  //
  nrLayer = 1;
  for(int i = d_mf->dd_nrLayer - 1; i >= 0; i--){

    // only layer have boundary values
    if(d_mf->dd_isConfined.at(i) == false){
      content << "EXTERNAL " << d_external_unit_number_heads << " 1.0 (FREE) -1 STRT Layer " << nrLayer <<"\n";
      nrLayer++;
    }
  }
  d_mf->d_cmethods->writeToFile(mf::execution_path(path, "pcrmf.ba6"), content.str());
}


void BAS::write_bound_array(std::string const& path) const {

  std::stringstream content;

  size_t nrLayer = 1;
  for(int i = d_mf->dd_nrLayer - 1; i >= 0; i--){

    // only layer have boundary values
    size_t pos = 0;
    if(d_mf->dd_isConfined.at(i) == false){
      for(size_t j = 0; j < d_mf->d_nrOfRows; j++){
        for(size_t k = 0; k < d_mf->d_nrOfColumns; k++){
          content << d_mf->d_ibound->cell(pos)[i] << " ";
          pos++;
        }
        content << std::endl;
      }
      nrLayer++;
    }
  }
  d_mf->d_cmethods->writeToFile(mf::execution_path(path, "pcrmf_bounds.asc"), content.str());
}


void BAS::write_head_array(std::string const& path) const {

  std::stringstream content;

  for(int i = d_mf->dd_nrLayer - 1; i >= 0; i--){
    d_mf->d_cmethods->writeMatrix2(content, d_mf->d_layer2BlockLayer, *(d_mf->d_initialHead), i);
  }
  d_mf->d_cmethods->writeToFile(mf::execution_path(path, "pcrmf_heads.asc"), content.str());
}



