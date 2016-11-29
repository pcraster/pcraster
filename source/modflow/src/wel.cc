#ifndef INCLUDED_WEL
#include "wel.h"
#define INCLUDED_WEL
#endif


// Library headers.
#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

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

#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
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
WEL::~WEL(){
}

/**
* Constructor
*/
WEL::WEL(PCRModflow *mf) :
  d_mf(mf),
  d_nr_wel_cells(0),
  d_output_unit_number(0),
  d_input_unit_number(280){//,
  //d_fortran_unit_number(0)

}

/**
* set well values by pcr maps
*/
bool WEL::setWell(const float *values, size_t layer){
  d_mf->d_methodName = "setWell";
  return d_mf->setBlockData(*(d_mf->d_welValues), values, layer);
}

void WEL::setWell(const calc::Field *well, size_t layer){
  layer--; // layer number passed by user starts with 1
  d_mf->d_gridCheck->isGrid(layer, "setWell");
  d_mf->d_gridCheck->isConfined(layer, "setWell");
  d_mf->d_gridCheck->testMV(well->src_f(), "setWell");
  setWell(well->src_f(), layer);
}


/**
* set well values by pcr block
*/
void WEL::setWell(const discr::BlockData<REAL4> &well){
d_mf->d_cmethods->setDiscrBlockData(well, *(d_mf->d_welValues));
}


/**
* write WEL to file
*/
// void WEL::writeWEL() const{
//
//   int mfLayer = 1;
//   int mfCount = 0;
//   std::stringstream content;
//   std::stringstream drnStr;
//
//   for(size_t layer = 1; layer <= d_mf->d_nrMFLayer; layer++){
//     size_t count = 0;
//     size_t size = d_mf->d_layer2BlockLayer.size();
//     size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - layer);
//     for (size_t row = 0; row < d_mf->d_nrOfRows; row++){
//       for (size_t col = 0; col < d_mf->d_nrOfColumns; col++){
//         double val = d_mf->d_welValues->cell(count)[blockLayer];
//         if(std::fabs(val - 0.0) > 0.00001){
//           drnStr << std::setw(10) << mfLayer;
//           drnStr << std::setw(10) << (row + 1);
//           drnStr << std::setw(10) << (col + 1);
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
//   // MXACTW IWELCB
//   content << std::setw(10) << mfCount;
//   content << std::setw(10) << d_fortran_unit_number;
//   content << std::setw(10) << "NOPRINT" << std::endl;
//   // ITMP NP
//   content << std::setw(10) << mfCount;
//   content << std::setw(10) << 0 << std::endl;
//   content << drnStr.str();
//
//   d_mf->d_cmethods->writeToFile("pcrmf.wel", content.str());
// }


calc::Field* WEL::get_well(size_t layer, std::string const& path){
  layer--;
  d_mf->d_gridCheck->isGrid(layer, "get_well");
  d_mf->d_gridCheck->isConfined(layer, "get_well");

  const std::string desc("           WELLS");
  std::stringstream stmp;
  stmp << "Can not open file containing WEL cell-by-cell flow terms";

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


void WEL::write_list(std::string const& path){

  std::string filename = mf::execution_path(path, "pcrmf_wel.asc");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  int mfLayer = 1;

  for(size_t layer = 1; layer <= d_mf->d_nrMFLayer; layer++){
    size_t count = 0;
    size_t size = d_mf->d_layer2BlockLayer.size();
    size_t blockLayer = d_mf->d_layer2BlockLayer.at(size - layer);
    for (size_t row = 0; row < d_mf->d_nrOfRows; row++){
      for (size_t col = 0; col < d_mf->d_nrOfColumns; col++){
        double val = d_mf->d_welValues->cell(count)[blockLayer];
        if(std::fabs(val - 0.0) > 0.00001){
          content << mfLayer;
          content << " " << (row + 1);
          content << " " << (col + 1);
          content << " " << val << "\n";
          d_nr_wel_cells++;
        }
        count++;
      }
    }
    mfLayer++;
  }
}


void WEL::write(std::string const& path){

  // # wel cells is calculated by write_list
  assert(d_nr_wel_cells != 0);

  std::string filename = mf::execution_path(path, "pcrmf.wel");

  std::ofstream content(filename);

  if(!content.is_open()){
    std::cerr << "Can not write " << filename << std::endl;
    exit(1);
  }

  content << "# Generated by PCRaster Modflow\n";
  // MXACTW IWELCB
  content << d_nr_wel_cells;
  content << " " << d_output_unit_number;
  content << " NOPRINT\n";
  // ITMP NP
  content << d_nr_wel_cells;
  content << " 0\n";
  content << "EXTERNAL " << d_input_unit_number << "\n";

  d_nr_wel_cells = 0;
}
