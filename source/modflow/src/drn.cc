#ifndef INCLUDED_DRN
#include "drn.h"
#define INCLUDED_DRN
#endif


// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
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
DRN::~DRN(){
}

/**
* Constuctor
*/
DRN::DRN(PCRModflow *mf) :
  d_mf(mf),
  d_drainUpdated(false),
  d_nr_drain_cells(0),
  d_output_unit_number(271),
  d_input_unit_number(270){
}


bool DRN::drainUpdated() const
{
  return d_drainUpdated;
}

void DRN::setDrainUpdated(bool value){
  d_drainUpdated = value;
}

/**
* setting drain values by pcr maps
*/
bool DRN::setDrain(const float *elevation, const float *conductance, size_t layer){
  d_mf->d_methodName = "setDrain elevation values";
  bool result = d_mf->setBlockData(*(d_mf->d_drnElev), elevation, layer);
  d_mf->d_methodName = "setDrain conductance values";
  result = d_mf->setBlockData(*(d_mf->d_drnCond), conductance, layer);
  d_drainUpdated = true;
  return result;
}

void DRN::setDrain(const calc::Field *elevation, const calc::Field *conductance, size_t layer){
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "setDrain");
  d_mf->d_gridCheck->isConfined(layer, "setDrain");
  d_mf->d_gridCheck->testMV(elevation->src_f(), "setDrain elevation");
  d_mf->d_gridCheck->testMV(conductance->src_f(), "setDrain conductance");
  setDrain(elevation->src_f(), conductance->src_f(), layer);
  d_drainUpdated = true;
}

/**
* setting drain values by pcr blocks
*/
void DRN::setDrain(const discr::BlockData<REAL4> &elevation, const discr::BlockData<REAL4> &conductance){
  d_mf->d_cmethods->setDiscrBlockData(elevation, *(d_mf->d_drnElev));
  d_mf->d_cmethods->setDiscrBlockData(conductance, *(d_mf->d_drnCond));
  d_drainUpdated = true;
}



/**
 * retrieving drain cell-by-cell flow values from the binary modflow output
 * format of the file is:
 * rec1 : header informations (label, top layer, nrRows, ...)
 * rec2 : flow values values for all layer
 */
// void DRN::getDrainFromBinary(){
//   std::ifstream file("fort.170", std::ios::in | std::ios::binary);
//   if(!file.is_open()){
//     std::stringstream stmp;
//     stmp << "Can not open file containing drain cell-by-cell flow terms";
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
//   char *headerData = new char[headerSizeBytes];
//   file.read(headerData, headerSizeBytes );
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
//       REAL4 val = floatData[pos];
//       d_mf->d_drnResult->cell(i)[blockLayer] = val;
//       pos++;
//     }
//   }
//   file.close();
//   delete[] charData;
//   charData = NULL;
//   delete[] headerData;
//   headerData = NULL;
// }




/**
* returning drain output as PCR blockdata
*/
// discr::BlockData<REAL4>* DRN::getBlockCellByCellFlow(){
//   discr::BlockData<REAL4> *resultDrn = new discr::BlockData<REAL4>(d_mf->d_baseArea);
//   d_mf->d_cmethods->setDiscrBlockData(*(d_mf->d_drnResult), *resultDrn);
//   return resultDrn;
// }




/**
* writing DRN to file
*/
// bool DRN::writeDRN() const{
//   size_t count = 0;
//   float val = -1.0;
//   int mfLayer = 1;
//   int mfCount = 0;
//   std::stringstream content;
//   std::stringstream drnStr;
//
//   for(size_t layer = 1; layer <= d_mf->d_nrMFLayer; layer++){
//     count = 0;
//     size_t size = d_mf->d_layer2BlockLayer.size();
//     size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - layer);
//     for (size_t row = 0; row < d_mf->d_nrOfRows; row++){
//       for (size_t col=0; col < d_mf->d_nrOfColumns; col++){
//         val = d_mf->d_drnCond->cell(count)[blockLayer];
//         if(val>0.0){
//           drnStr << std::setw(10) << mfLayer;
//           drnStr << std::setw(10) << (row + 1);
//           drnStr << std::setw(10) << (col + 1);
//           drnStr << " ";
//           drnStr << std::setw(10) << d_mf->d_drnElev->cell(count)[blockLayer];
//           drnStr << " ";
//           drnStr << std::setw(10) << val;
//           drnStr << std::endl;
//           mfCount++;
//         }
//         count++;
//       }
//     }
//     mfLayer++;
//   }
//
//   // MXACTD IDRNB
//   content << std::setw(10) << mfCount;
//   content << std::setw(10) << d_output_unit_number;
//   content << std::setw(10) << "NOPRINT" << std::endl;
//   // ITMP NP
//   content << std::setw(10) << mfCount;
//   content << std::setw(10) << 0 << std::endl;
//
//   content << drnStr.str();
//
//   return d_mf->d_cmethods->writeToFile("pcrmf.drn", content.str());
//
// }


// calc::Field* DRN::getDrain(size_t layer){
//   layer--;
//   d_mf->d_gridCheck->isGrid(layer, "getDrain");
//   d_mf->d_gridCheck->isConfined(layer, "getDrain");
//
//   calc::Spatial* spatial = new calc::Spatial(VS_S, calc::CRI_f, d_mf->d_nrOfCells);
//   REAL4* cells = static_cast<REAL4*>(spatial->dest());
//
//   for(size_t pos = 0; pos < d_mf->d_nrOfCells; pos++){
//     REAL4 value = d_mf->d_drnResult->cell(pos)[layer];
//       cells[pos] = static_cast<REAL4>(value);
//   }
//   return spatial;
// }




/**
* writing drain output to PCR map
*/
void DRN::getDrain(float *values, size_t layer, std::string const& path) const {
  d_mf->d_gridCheck->isGrid(layer, "getDrain");
  d_mf->d_gridCheck->isConfined(layer, "getDrain");

  const std::string desc("          DRAINS");
  std::stringstream stmp;
  stmp << "Can not open file containing DRAINS cell-by-cell flow terms";

  // modflow reports from top to bottom, thus
  // get the 'inverse' layer number to start from the right position
  int pos_multiplier = d_mf->get_modflow_layernr(layer);

  mf::BinaryReader reader;
  const std::string filename(mf::execution_path(path, "fort." + boost::lexical_cast<std::string>(d_output_unit_number)));
  reader.read(stmp.str(), filename, values, desc, pos_multiplier);
}



calc::Field* DRN::getDrain(size_t layer, std::string const& path) const {
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "getDrain");
  d_mf->d_gridCheck->isConfined(layer, "getDrain");

  const std::string desc("          DRAINS");
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




void DRN::write(std::string const& path) const{

  // # drn cells is calculated by write_list
  assert(d_nr_drain_cells != 0);

  std::string filename = mf::execution_path(path, "pcrmf.drn");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }


  content << "# Generated by PCRaster Modflow" << std::endl;


  // MXACTD IDRNB
  content << d_nr_drain_cells;
  content << " " << d_output_unit_number;
  content << " " << "NOPRINT\n";
  // ITMP NP
  content << d_nr_drain_cells << " 0\n";
  content << "EXTERNAL " << d_input_unit_number << "\n";

}


void DRN::write_list(std::string const& path) {

  std::string filename = mf::execution_path(path, "pcrmf_drn.asc");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;

    exit(1);
  }


  size_t count = 0;
  float val = -1.0;
  int mfLayer = 1;

  for(size_t layer = 1; layer <= d_mf->d_nrMFLayer; layer++){
    count = 0;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - layer);
    for (size_t row = 0; row < d_mf->d_nrOfRows; row++){
      for (size_t col=0; col < d_mf->d_nrOfColumns; col++){
        val = d_mf->d_drnCond->cell(count)[blockLayer];
        if(val>0.0){
          content << mfLayer;
          content << " " << (row + 1);
          content << " " << (col + 1);
          content << " " << d_mf->d_drnElev->cell(count)[blockLayer];
          content << " " << val;
          content << "\n";
          d_nr_drain_cells++;
        }
        count++;
      }
    }
    mfLayer++;
  }
}
