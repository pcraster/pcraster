#ifndef INCLUDED_GRIDCHECK
#include "gridcheck.h"
#define INCLUDED_GRIDCHECK
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
GridCheck::~GridCheck(){
}

/**
 * Constructor
 */
GridCheck::GridCheck(PCRModflow *mf) : d_mf(mf){
}

/**
 * some simple test for assigning methods to MF layer
 */
void GridCheck::isGrid(size_t layer, const std::string &methodName){
  // maximum layer number
  
  size_t size = d_mf->dd_isConfined.size();// - 1;
  if(0 == size){
    d_mf->d_cmethods->error("Grid not yet defined: No layer specified", methodName);
  }
  if(layer > size){
    std::stringstream stmp;
    stmp << "Operation on layer " << static_cast<int>(layer + 1) << " failed: Layer number must be between 1 and " << (size) ;
    d_mf->d_cmethods->error(stmp.str(), methodName);
  }
}

/**
 * tests if layer is a confining bed
 * program will exit if user tries to get/set values from/to a confining bed
 * \param layer layer number
 * \param methodName name of calling method
 */
void GridCheck::isConfined(size_t layer, const std::string &methodName){
  if(d_mf->dd_isConfined.at(layer) == true){
    std::stringstream stmp;
    stmp << "Operation failed: Layer " << static_cast<int>(layer + 1) << " is specified as confining bed";
    d_mf->d_cmethods->error(stmp.str(), methodName);
  }
}

/**
 * vertical conductivity can be set on each layer in the block
 * \todo test if grid exists...
 * \todo still needed?
 */
void GridCheck::setVCond(size_t mfLayer, const std::string &methodName){
  size_t size = d_mf->d_nrBlockLayer - 1;
  if(mfLayer>size){
    std::stringstream stmp;
    stmp << "Operation on layer " << mfLayer << " failed: Maximum layer number is " << size;
    d_mf->d_cmethods->error(stmp.str(), methodName);
  }
}

/**
 * test for missing values
 */
void GridCheck::testMV(const float *values, const std::string &methodName){
  size_t size = d_mf->d_nrOfCells;
  for(size_t i = 0; i < size; ++i){
    if(IS_MV_REAL4(values + i)){
      size_t row = 1 + i / d_mf->d_nrOfColumns;
      size_t col = 1 + i % d_mf->d_nrOfColumns;
      std::stringstream stmp;
      stmp << "Missing value detected in row " << row << " column " << col; 
      d_mf->d_cmethods->error(stmp.str(), methodName);
    }
  }
}

/**
 * test for missing values
 */
void GridCheck::testMV(const int *values, const std::string &methodName){

  size_t size = d_mf->d_nrOfCells;
  for(size_t i = 0; i < size; ++i){
    if(IS_MV_INT4(values + i)){
      size_t row = 1 + i / d_mf->d_nrOfColumns;
      size_t col = 1 + i % d_mf->d_nrOfColumns;
      std::stringstream stmp;
      stmp << "Missing value detected in row " << row << " column " << col;
      d_mf->d_cmethods->error(stmp.str(), methodName);
    }
  }
}

/**
 * testing thickness of layer
 */
void GridCheck::testElevation(){
  size_t layerSize = d_mf->d_nrBlockLayer;
  size_t cellSize = d_mf->d_nrOfCells;

  for(size_t layer = 0; layer < layerSize; layer++){
    for(size_t i = 0; i < cellSize; i++){
      if(d_mf->d_baseArea->cell(i)[layer] < 0.0){
        std::stringstream stmp;
  stmp << "Grid specification: Thickness of layer " << layer << " less than 0";
        d_mf->d_cmethods->error(stmp.str(), "run");
      }
    }
  }
}
