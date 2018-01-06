#include "pickle.h"
#include <boost/format.hpp>
#include <boost/algorithm/string.hpp>
#include "calc_spatial.h"
#include "calc_field.h"
#include "pcrtypes.h"
#include "calc_map2csf.h"
#include "Globals.h"


namespace pcraster {
namespace python {

calc::Field* initField(int value_scale,
         int cri,
         size_t nr_rows,
         size_t nr_cols,
         double north,
         double west,
         double cell_size,
         int projection,
         int version){
    // function call and arguments here should just come from the unpickling suite
    // thus, only POD arguments are given (due to string representation in pickle)
    (void)version;  // shut up compiler

    size_t nr_cells = nr_rows * nr_cols;
    VS vs = static_cast<VS>(value_scale);
    CSF_VS csf_value_scale = calc::vs2CsfVs(vs);

    calc::Spatial* field = new calc::Spatial(vs, (calc::CRIndex)cri, nr_cells);

    switch(csf_value_scale) {
      case VS_BOOLEAN:
      case VS_LDD: {
        INT1* cells = static_cast<INT1*>(field->dest());
        (void)cells;  // shut up compiler
        break;
      }
      case VS_NOMINAL:
      case VS_ORDINAL: {
        INT4* cells = static_cast<INT4*>(field->dest());
        (void)cells;  // shut up compiler
        break;
      }
      case VS_SCALAR:
      case VS_DIRECTION: {
        REAL4* cells = static_cast<REAL4*>(field->dest());
        (void)cells;  // shut up compiler
        break;
      }
      default: {
        throw std::invalid_argument((boost::format("unable to create a raster with valuescale %1%")
         % csf_value_scale).str());
        break;
      }
    }

    if(!globals.cloneSpace().valid()) {
      geo::RasterSpace cloneSpace(nr_rows, nr_cols, cell_size, west, north, static_cast<geo::Projection>(projection));
      globals.setCloneSpace(cloneSpace);
    }

    assert(globals.cloneSpace().valid());

    return field;
}


template<class T>
void str_values(std::stringstream & content, calc::Field const & raster){
  // TODO: consider hexfloats when we use a C++11 compiler
  double val;

  for(size_t i = 0 ; i < raster.nrValues(); ++i){
    raster.getCell(val, i);
    // put m for MV, this will cause a desired throw
    // from lexical_cast on number conversion
    if(pcr::isMV(val)){
      content << "m ";
    }
    else{
      content << static_cast<T>(val) << " ";
    }
  }
}


template<class T>
void fill_raster(calc::Field & field, const boost::python::tuple state){
  // unpack the values from the state string
  std::vector<std::string> values;

  std::string s = boost::python::extract<std::string>(state[0]);
  boost::trim(s);

  boost::split(values, s, boost::is_any_of(" "), boost::token_compress_on);
  assert(values.size() == field.nrValues());

  T* cells = static_cast<T*>(field.dest());
  size_t count = 0;

  for(std::vector<std::string>::iterator it = values.begin(); it != values.end(); ++it){
    try{
      cells[count] = boost::lexical_cast<T>(*it);
    }
    catch(const boost::bad_lexical_cast &){
      pcr::setMV(cells[count]);
    }
    count ++;
  }
}


boost::python::tuple field_pickle_suite::getinitargs(calc::Field const & raster){
  // not sure if we ever need it, but include a version number
  // of the PCRaster pickle format
  // maybe in case we switch to hexfloats
  int version = 1;   // v1 initial version

  size_t nr_rows = globals.cloneSpace().nrRows();
  size_t nr_cols = globals.cloneSpace().nrCols();
  double north = globals.cloneSpace().north();
  int projection = static_cast<int>(globals.cloneSpace().projection());
  double west = globals.cloneSpace().west();
  double cell_size = globals.cloneSpace().cellSize();
  int vs = static_cast<int>(raster.vs());
  int cri = static_cast<int>(raster.cri());

  return boost::python::make_tuple(vs, cri, nr_rows, nr_cols, north, west, cell_size, projection, version);
}


boost::python::tuple field_pickle_suite::getstate(calc::Field const & raster){
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
      throw std::invalid_argument((boost::format("unable to pickle a raster with datatype '%1%'")
       % value_scale).str());
      break;
    }
  }

  return boost::python::make_tuple(values.str());
}


void field_pickle_suite::setstate(calc::Field & field, const boost::python::tuple state){
  CSF_VS csf_value_scale = calc::vs2CsfVs(field.vs());

  switch(csf_value_scale){
    case VS_BOOLEAN:
    case VS_LDD: {
      fill_raster<INT1>(field, state);
      break;
    }
    case VS_NOMINAL:
    case VS_ORDINAL: {
      fill_raster<INT4>(field, state);
      break;
    }
    case VS_SCALAR:
    case VS_DIRECTION: {
      fill_raster<REAL4>(field, state);
      break;
    }
    default: {
      throw std::invalid_argument((boost::format("unable to create a raster with valuescale %1%")
       % csf_value_scale).str());
      break;
    }
  }
}

} // namespace python
} // namespace pcraster
