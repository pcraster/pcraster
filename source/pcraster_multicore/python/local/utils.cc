#include <cassert>
#include <sstream>

#include <cmath>

#include "calc_nonspatial.h"
#include "calc_spatial.h"
#include "pcrdatatype.h"
#include "Globals.h"
#include "appargs.h"

#include "fern/algorithm/policy/execution_policy.h"

#include "pcraster_multicore/python/type_conversion/type_conversion.h"
#include "pcraster_multicore/python/local/mul.h"
#include "pcraster_multicore/python/execution_policy.h"
#include "pcraster_multicore/wrapper/multicore_spatial.h"
#include "pcraster_multicore/wrapper/multicore_nonspatial.h"



namespace fa = fern::algorithm;

namespace pcraster_multicore {
namespace python {




size_t nr_rows(){
  return pcraster::python::globals.cloneSpace().nrRows();
}
size_t nr_cols(){
  return pcraster::python::globals.cloneSpace().nrCols();
}
size_t nr_cells(){
  return pcraster::python::globals.cloneSpace().nrRows() * pcraster::python::globals.cloneSpace().nrCols();
}



bool boolean_valuescale(const calc::Field& aField){
  PCR_VS field_vs = VS_UNKNOWN;
  field_vs = aField.vs();
  return field_vs == VS_B ? true : false;
}
bool ldd_valuescale(const calc::Field& aField){
  PCR_VS field_vs = VS_UNKNOWN;
  field_vs = aField.vs();
  return field_vs == VS_L ? true : false;
}

bool nominal_valuescale(const calc::Field& aField){
  PCR_VS field_vs = VS_UNKNOWN;
  field_vs = aField.vs();
  return field_vs == VS_N ? true : false;
}

bool ordinal_valuescale(const calc::Field& aField){
  PCR_VS field_vs = VS_UNKNOWN;
  field_vs = aField.vs();
  return field_vs == VS_O ? true : false;
}

bool scalar_valuescale(const calc::Field& aField){
  PCR_VS field_vs = VS_UNKNOWN;
  field_vs = aField.vs();
  return field_vs == VS_S ? true : false;
}

bool directional_valuescale(const calc::Field& aField){
  PCR_VS field_vs = VS_UNKNOWN;
  field_vs = aField.vs();
  return field_vs == VS_D ? true : false;
}





void assert_equal_valuescale(const calc::Field& field_a, const calc::Field& field_b, const std::string& msg){
  PCR_VS field_vs1 = VS_UNKNOWN;
  PCR_VS field_vs2 = VS_UNKNOWN;
  field_vs1 = field_a.vs();
  field_vs2 = field_b.vs();
  if(field_vs1 != field_vs2){
    std::stringstream err_msg{};
    err_msg << msg << " is of type '" << field_vs2 << "', while other is of type '" << field_vs1 << "'\n";
    throw std::runtime_error(err_msg.str());
  }
}


void assert_boolean_valuescale(const calc::Field& aField, const std::string& msg){

  PCR_VS field_vs = VS_UNKNOWN;
  field_vs = aField.vs();

  if(field_vs != VS_B){
    std::stringstream err_msg{};
    err_msg << msg << " is of type '" << field_vs << "', legal type is 'boolean'\n";
    throw std::runtime_error(err_msg.str());
  }
}

void assert_scalar_valuescale(const calc::Field& aField, const std::string& msg){

  PCR_VS field_vs = VS_UNKNOWN;
  field_vs = aField.vs();

  if(field_vs != VS_S){
    std::stringstream err_msg{};
    err_msg << msg << " is of type '" << field_vs << "', legal type is 'scalar'\n";
    throw std::runtime_error(err_msg.str());
  }
}


void assert_ordinal_valuescale(const calc::Field& aField, const std::string& msg){

  PCR_VS field_vs = VS_UNKNOWN;
  field_vs = aField.vs();

  if(field_vs != VS_O){
    std::stringstream err_msg{};
    err_msg << msg << " is of type '" << field_vs << "', legal type is 'ordinal'\n";
    throw std::runtime_error(err_msg.str());
  }
}


void assert_nominal_valuescale(const calc::Field& aField, const std::string& msg){

  PCR_VS field_vs = VS_UNKNOWN;
  field_vs = aField.vs();

  if(field_vs != VS_N){
    std::stringstream err_msg{};
    err_msg << msg << " is of type '" << field_vs << "', legal type is 'nominal'\n";
    throw std::runtime_error(err_msg.str());
  }
}


calc::Field* degrees_to_radians(const multicore_field::Nonspatial<REAL4>* aField,  multicore_field::Nonspatial<REAL4>* result){
  double conversion_factor = M_PI/180.0;
  calc::Field* field_conv = new calc::NonSpatial(VS_S, conversion_factor);
  multicore_field::Nonspatial<REAL4> deg_rad(field_conv);
  return mul_number_number(aField,&deg_rad, result);
}

calc::Field* degrees_to_radians(const multicore_field::Spatial<REAL4>* aField,  multicore_field::Spatial<REAL4>* result){
  fa::ExecutionPolicy epol = execution_policy();
  double conversion_factor = M_PI/180.0;
  calc::Field* field_conv = new calc::NonSpatial(VS_S, conversion_factor);
  multicore_field::Nonspatial<REAL4> deg_rad(field_conv);
  return mul_field_number(epol,aField,&deg_rad, result);
}






bool global_option_degrees(){
  return appDirection == APP_DEGREES ? true : false;
}


bool global_option_unittrue(){
  return appUnitTrue == TRUE ? true : false;
}

void assert_equal_location_attributes(const calc::Field& field){
  // this is a rather weak test, we should add methods to the PCRaster calc::Field
  // that expose location attributes
  // on the other hand, this test is similar to the one performed by the
  // 4.1.0 PCRaster model engine...
  if(field.isSpatial() && (nr_cells() != field.nrValues())){
    throw std::runtime_error("Number of cells is different from clone map");
  }
}



calc::Field* to_scalar(calc::Field* nonspatial){
  if(scalar_valuescale(*nonspatial)){
    return nonspatial;
  }
  else{
    return scalar(nonspatial);
  }
}


calc::Field* to_boolean(calc::Field* nonspatial){
  if(boolean_valuescale(*nonspatial)){
    return nonspatial;
  }
  else{
    return boolean(nonspatial);
  }
}


calc::Field* to_ordinal(calc::Field* nonspatial){
  if(ordinal_valuescale(*nonspatial)){
    return nonspatial;
  }
  else{
    return ordinal(nonspatial);
  }
}




} // namespace python
} // namespace pcraster_multicore

