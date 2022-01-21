#include "pickle.h"
#include "calc_spatial.h"
#include "calc_field.h"
#include "pcrtypes.h"
#include "calc_map2csf.h"
#include "Globals.h"
#include <boost/algorithm/string.hpp>
#include <sstream>
#include <ios>


namespace pcraster {
namespace python {

template<class T>
void str_values(std::stringstream & content, calc::Field const & raster){
  double val;

  for(size_t i = 0 ; i < raster.nrValues(); ++i){
    raster.getCell(val, i);
    // Would it be safe to hexfloat the MV values instead of this
    // string workaround?
    if(pcr::isMV(val)){
      content << "m ";
    }
    else{
      content << std::hexfloat << val << " ";
    }
  }
}


template<class T>
void fill_raster(calc::Field & field, const pybind11::tuple& state){
  // unpack the values from the state string

  std::vector<std::string> values;

  auto s = state[0].cast<std::string>();
  boost::trim(s);
  boost::split(values, s, boost::is_any_of(" "), boost::token_compress_on);

  assert(values.size() == field.nrValues());

  T* cells = static_cast<T*>(field.dest());

  size_t count = 0;

  for(auto & value : values){
    if (value != "m") {
      // Correct direct parsing of hexstrings seems to be still discussed
      // without strtod reading hexstring will result in 0
      // see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81122#c1
      cells[count] = static_cast<T>(std::strtod(value.c_str(), nullptr));
    }
    else {
      pcr::setMV(cells[count]);
    }

    count ++;
  }
}


pybind11::tuple getstate(calc::Field const & raster){

  CSF_VS value_scale = calc::vs2CsfVs(raster.vs());
  std::stringstream values;
  values.precision(std::numeric_limits<double>::digits10 + 1);

  switch(value_scale){
    case VS_BOOLEAN:
    case VS_LDD:{
      str_values<UINT1>(values, raster);
      break;
    }
    case VS_NOMINAL:
    case VS_ORDINAL: {
      str_values<INT4>(values, raster);
      break;
    }
    case VS_SCALAR:
    case VS_DIRECTION: {
      str_values<REAL4>(values, raster);
      break;
    }
    default: {
      std::ostringstream errMsg;
      errMsg << "unable to pickle a raster with datatype '"
             << value_scale
             << "'";
      throw std::invalid_argument(errMsg.str());
      break;
    }
  }

  size_t nr_rows = globals.cloneSpace().nrRows();
  size_t nr_cols = globals.cloneSpace().nrCols();
  double north = globals.cloneSpace().north();
  int projection = static_cast<int>(globals.cloneSpace().projection());
  double west = globals.cloneSpace().west();
  double cell_size = globals.cloneSpace().cellSize();
  int vs = static_cast<int>(raster.vs());
  int cri = static_cast<int>(raster.cri());

  return pybind11::make_tuple(values.str(), vs, cri, nr_rows, nr_cols, north, west, cell_size, projection);
}


calc::Field* setstate(pybind11::tuple const & state) {
  auto nr_rows = state[3].cast<size_t>();
  auto nr_cols = state[4].cast<size_t>();
  size_t nr_cells = nr_rows * nr_cols;
  auto north = state[5].cast<double>();
  auto west = state[6].cast<double>();
  auto cell_size = state[7].cast<double>() ;
  int projection = state[8].cast<int>();

  if (!globals.cloneSpace().valid()) {
    geo::RasterSpace cloneSpace(nr_rows, nr_cols, cell_size, west, north, static_cast<geo::Projection>(projection));
    globals.setCloneSpace(cloneSpace);
  }
  else {
    if ((globals.cloneSpace().nrRows() != nr_rows) || (globals.cloneSpace().nrCols() != nr_cols)) {
      std::ostringstream errMsg;
      errMsg << "number of rows and columns ("
             << nr_rows
             << ", "
             << nr_cols
             << ") differ from currently used ("
             << globals.cloneSpace().nrRows()
             << ", "
             << globals.cloneSpace().nrCols()
             << ")\n";
      throw std::invalid_argument(errMsg.str());
    }
    if ((globals.cloneSpace().north() != north) || (globals.cloneSpace().west() != west)) {
      std::ostringstream errMsg;
      errMsg << "west and north ("
             << west
             << ", "
             << north
             << ") differ from currently used ("
             << globals.cloneSpace().west()
             << ", "
             << globals.cloneSpace().north()
             << ")\n";
      throw std::invalid_argument(errMsg.str());
    }
    if (globals.cloneSpace().cellSize() != cell_size) {
      std::ostringstream errMsg;
      errMsg << "cell size ("
             << cell_size
             << ") differs from currently used ("
             << globals.cloneSpace().cellSize()
             << ")\n";
      throw std::invalid_argument(errMsg.str());
    }
  }

  VS vs = static_cast<VS>(state[1].cast<int>());
  calc::CRIndex cri = static_cast<calc::CRIndex>(state[2].cast<int>());

  calc::Field *field = new calc::Spatial(vs, cri, nr_cells);

  CSF_VS csf_value_scale = calc::vs2CsfVs(vs);

  switch(csf_value_scale){
    case VS_BOOLEAN:
    case VS_LDD: {
      fill_raster<UINT1>(*field, state);
      break;
    }
    case VS_NOMINAL:
    case VS_ORDINAL: {
      fill_raster<INT4>(*field, state);
      break;
    }
    case VS_SCALAR:
    case VS_DIRECTION: {
      fill_raster<REAL4>(*field, state);
      break;
    }
    default: {
      std::ostringstream errMsg;
      errMsg << "unable to create a raster with valuescale '"
             << csf_value_scale
             << "'";
      throw std::invalid_argument(errMsg.str());
      break;
    }
  }

  return field;
}

} // namespace python
} // namespace pcraster
