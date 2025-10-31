#include "mf_BinaryReader.h"
#include "pcrmodflow.h"
#include "gridcheck.h"
#include "common.h"

#include <cstddef>
#include <sstream>
#include <iostream>
#include <fstream>

/*!
  \file
  This file contains the implementation of the BinaryReader class.
*/

namespace {

} // Anonymous namespace



namespace mf {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BINARYREADER MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF BINARYREADER MEMBERS
//------------------------------------------------------------------------------

BinaryReader::BinaryReader()
{
}



BinaryReader::~BinaryReader()
{
}
void BinaryReader::read(const std::string & err_mgs, const std::string & filename, float *values, const std::string& description, size_t multiplier) const {
//void BinaryReader::read(const std::string & err_mgs, int unit_number, float *values, const std::string description, size_t multiplier) const {
  // see also flow data description at faq how to read binary
  // http://water.usgs.gov/nrp/gwsoftware/modflow2000/Guide/index.html

  //std::string filename("fort." + boost::lexical_cast<std::string>(unit_number));

  std::ifstream file(filename.c_str(), std::ios::in | std::ios::binary);
  if(!file.is_open()){
    std::cerr << "Error in PCRasterModflow: " << '\n' << "  ";
    std::cerr << err_mgs << '\n';
    //d_mf->d_cmethods->errorMessage(err_mgs, "run");
    exit(1);
  }

  //size_t nr_cells = d_mf->d_nrOfCells;
  //int nr_result_layer = d_mf->nr_modflow_layer();
  int nr_bytes = sizeof(float);

  // first we should check if the requested content is at 'that' position...
  // 36 metadata; 16 2 * block markers
  //int skip_bytes_until_block = mf::recordMarkerSize + start * (36 + 16 + nr_cells * nr_result_layer * nr_bytes);

  file.seekg(mf::recordMarkerSize);

  char tmp[4];
  file.read(tmp, 4);  // dummy read kstep
  file.read(tmp, 4);  // dummy read kper

  //char* desc = new char[17];  // desc contains the array description
  char desc[17];
  file.read(desc, 16);
  desc[16] = '\0';

  //std::cout << desc << std::endl;
  //std::cout << description << std::endl;

   if(description.compare(desc) != 0){
     std::cerr << "Error in PCRasterModflow: " << '\n';
     //std::stringstream stmp;
     std::cerr << "  Cannot find " << description << " in the output file " << filename << '\n';
     //d_mf->d_cmethods->errorMessage(stmp.str(), "run");
     exit(1);
   }

  int col = 0;
  file.read(tmp, 4);
  std::memcpy(&col, &(tmp[0]), 4);

  int row = 0;
  file.read(tmp, 4);
  std::memcpy(&row, &(tmp[0]), 4);

  int lay= 0;
  file.read(tmp, 4);
  std::memcpy(&lay, &(tmp[0]), 4);

  size_t nr_cells = static_cast<size_t>(row * col);

  // jump to the right block and position, skip the metadata, block marker;
  // multiplier holds layer number of the layer we are interested in
  size_t new_pos = nr_bytes + 36 + 8 + multiplier * (nr_cells * nr_bytes);


  //size_t nr_cells = row * col;
  file.seekg(new_pos);

  char *charData = new char[nr_cells * nr_bytes];
  file.read(charData, nr_cells * nr_bytes);
  auto *floatData = reinterpret_cast<float *>(charData);

  for(size_t pos = 0; pos < nr_cells; ++pos){
      values[pos] = floatData[pos];
  }

  file.close();

  //delete[] desc;
  //desc = NULL;

  delete[] charData;
  charData = nullptr;
//}

}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace mf

