#include "common.h"
#include "pcrmodflow.h"

#include <fstream>
#include <iostream>
#include <iomanip>
#include <ios>


template
void Common::writeMatrix<float>(std::stringstream &aStream, const std::string &aString, const discr::BlockData<float> &bdata, size_t layer);
template
void Common::writeMatrix<int>(std::stringstream &aStream, const std::string &aString, const discr::BlockData<int> &bdata, size_t layer);
template
void Common::setDiscrBlockData(const discr::BlockData<REAL4> &source, discr::BlockData<REAL4> &result);
template
void Common::setDiscrBlockData(const discr::BlockData<INT4> &source, discr::BlockData<INT4> &result);

/**
* Destructor
*/
Common::~Common(){
}


/**
* Constructor
*/
Common::Common(PCRModflow *mf) : d_mf(mf){
}


/**
* writing string to file
*/
bool Common::writeToFile(const std::string& filename, const std::string& msg){
  std::ofstream file(filename.c_str());
  if(!file.is_open()){
    std::cerr << "Can not write " << filename << '\n';
    return false;
  }
  file << msg;
  file.close();
  return true;
}


/**
* writes matrix to file
*/
void Common::writeMatrix(std::stringstream& aStream, const std::string& aString, std::vector<int>& l2BlockLayer, const discr::BlockData<REAL4>& bdata, size_t layer){
  size_t count  = 0;
  auto position = std::find(l2BlockLayer.begin(), l2BlockLayer.end(), static_cast<int>(layer));
  if(position != l2BlockLayer.end()){
    aStream << aString << "\n";
    for(size_t j=0;j<d_mf->d_nrOfRows;j++){
      for(size_t k = 0; k<d_mf->d_nrOfColumns; k++){
        aStream  << bdata.cell(count)[layer] << " ";
        count++;
      }
      aStream << "\n";
    }
  }
}

void Common::writeMatrix2(std::stringstream& aStream, std::vector<int>& l2BlockLayer, const discr::BlockData<REAL4>& bdata, size_t layer){
  size_t count  = 0;
  auto position = std::find(l2BlockLayer.begin(), l2BlockLayer.end(), static_cast<int>(layer));
  if(position != l2BlockLayer.end()){
    for(size_t j=0;j<d_mf->d_nrOfRows;j++){
      for(size_t k = 0; k<d_mf->d_nrOfColumns; k++){
        aStream  << bdata.cell(count)[layer] << " ";
        count++;
      }
      aStream << "\n";
    }
  }
}

/**
 * recharge
 * \todo both or this?
*/
template<typename T>
void Common::writeMatrix(std::stringstream& aStream, const std::string& aString, const discr::BlockData<T>& bdata, size_t layer)
{
  size_t count = 0;
  aStream << aString << '\n';
  for(size_t j=0; j<d_mf->d_nrOfRows; j++){
    for(size_t k=0;k<d_mf->d_nrOfColumns;k++){
      aStream << " " << bdata.cell(count)[layer];
      count++;
    }
    aStream << "\n";
  }
}


/**
* printing error message
*/
void Common::error(const std::string &msg, const std::string &methodName){
  std::cerr << '\n' << "Error in PCRasterModflow: " << methodName << '\n';
  std::cerr << "  " << msg << '\n';
  std::exit(1);
}



template<typename T>
void Common::setDiscrBlockData(const discr::BlockData<T> &source, discr::BlockData<T> &result){
  for(size_t currLayer = 0; currLayer < d_mf->d_nrBlockLayer; currLayer++){
    for(size_t currCell = 0; currCell < d_mf->d_nrOfCells; currCell++){
      result.cell(currCell)[currLayer] = source.cell(currCell)[currLayer];
    }
  }
}

