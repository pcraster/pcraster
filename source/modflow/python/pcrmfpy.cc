#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif


#include "mf_ModflowPython.h"
#include <pybind11/pybind11.h>


using namespace pybind11::literals;


// Module headers.



// Wrapper for overloaded methods
bool (mf::PCRModflowPython::*createBottomCalc)(const float *, const float *) = &mf::PCRModflowPython::createBottom;
void (mf::PCRModflowPython::*createBottomPy)(const calc::Field *, const calc::Field *) = &mf::PCRModflowPython::createBottom;
void (mf::PCRModflowPython::*createBottomPS)(const std::string&, const std::string&) = &mf::PCRModflowPython::createBottomPS;
//void (mf::PCRModflowPython::*setLayer)(const discr::Block &, const discr::BlockData<INT4> &) = &mf::PCRModflowPython::setLayer;
bool (mf::PCRModflowPython::*addLayerCalc)(const float *) = &mf::PCRModflowPython::addLayer;
void (mf::PCRModflowPython::*addLayerPy)(const calc::Field *) = &mf::PCRModflowPython::addLayer;
void (mf::PCRModflowPython::*addLayerPS)(const std::string&) = &mf::PCRModflowPython::addLayerPS;

bool (mf::PCRModflowPython::*addConfinedCalc)(const float *) = &mf::PCRModflowPython::addConfinedLayer;
void (mf::PCRModflowPython::*addConfinedPy)(const calc::Field *) = &mf::PCRModflowPython::addConfinedLayer;
void (mf::PCRModflowPython::*addConfinedPS)(const std::string&) = &mf::PCRModflowPython::addConfinedLayerPS;

bool (mf::PCRModflowPython::*setBoundaryCalc)(const int *, size_t) = &mf::PCRModflowPython::setIBound;
void (mf::PCRModflowPython::*setBoundaryPS)(const std::string&, size_t) = &mf::PCRModflowPython::setIBound;
void (mf::PCRModflowPython::*setBoundaryPy)(const calc::Field *values, size_t layer) = &mf::PCRModflowPython::setIBound;
bool (mf::PCRModflowPython::*setBoundaryPyBlock)(const discr::BlockData<INT4> &) = &mf::PCRModflowPython::setIBound;

bool (mf::PCRModflowPython::*setHeadCalc)(const float *, size_t) = &mf::PCRModflowPython::setInitialHead;
void (mf::PCRModflowPython::*setHeadPS)(const std::string &, size_t) = &mf::PCRModflowPython::setInitialHead;
void (mf::PCRModflowPython::*setHeadPy)(const calc::Field *, size_t) = &mf::PCRModflowPython::setInitialHead;
bool (mf::PCRModflowPython::*setHeadPyBlock)(const discr::BlockData<REAL4> &) = &mf::PCRModflowPython::setInitialHead;


bool (mf::PCRModflowPython::*setHCond2)(const float *, size_t, size_t) = &mf::PCRModflowPython::setHCond;
void (mf::PCRModflowPython::*setHCond)(const discr::BlockData<REAL4> &, const discr::BlockData<INT4> &) = &mf::PCRModflowPython::setHCond;

//bool (mf::PCRModflowPython::*setVCond1)(const std::string &, size_t) = &mf::PCRModflowPython::setVCond;
bool (mf::PCRModflowPython::*setVCond2)(const float *, size_t) = &mf::PCRModflowPython::setVCond;
void (mf::PCRModflowPython::*setVCond)(const discr::BlockData<REAL4> &) = &mf::PCRModflowPython::setVCond;

bool (mf::PCRModflowPython::*setWettingCalc)(const float *, size_t) = &mf::PCRModflowPython::setWetting;
void (mf::PCRModflowPython::*setWettingPS)(const std::string &, size_t) = &mf::PCRModflowPython::setWetting;
void (mf::PCRModflowPython::*setWettingPy)(const calc::Field *, size_t) = &mf::PCRModflowPython::setWetting;
void (mf::PCRModflowPython::*setWettingPyBlock)(const discr::BlockData<REAL4> &) = &mf::PCRModflowPython::setWetting;



//bool (mf::PCRModflowPython::*setRiv1)(const std::string &, const std::string &, const std::string &, size_t) = &mf::PCRModflowPython::setRiver;
bool (mf::PCRModflowPython::*setRiv2)(const float *, const float *, const float *, size_t) = &mf::PCRModflowPython::setRiver;
void (mf::PCRModflowPython::*setRiv)(discr::BlockData<REAL4> &, discr::BlockData<REAL4> &, discr::BlockData<REAL4> &) = &mf::PCRModflowPython::setRiver;

void (mf::PCRModflowPython::*setRecharge1)(const float *, size_t) = &mf::PCRModflowPython::setRecharge;
void (mf::PCRModflowPython::*setRechargeLay1)(const float *, const int *) = &mf::PCRModflowPython::setRechargeLay;
void (mf::PCRModflowPython::*setRecharge)(const calc::Field *, size_t) = &mf::PCRModflowPython::setRecharge;
void (mf::PCRModflowPython::*setRechargeLay)(const calc::Field *, const calc::Field *) = &mf::PCRModflowPython::setRechargeLay;
void (mf::PCRModflowPython::*setRechargePS)(const std::string &, size_t) = &mf::PCRModflowPython::setRecharge;
void (mf::PCRModflowPython::*setRechargeLayPS)(const std::string &, const std::string &) = &mf::PCRModflowPython::setRechargeLay;

bool (mf::PCRModflowPython::*setRiverCalc)(const float *, const float *, const float *, size_t) = &mf::PCRModflowPython::setRiver;
void (mf::PCRModflowPython::*setRiverPS)(const std::string &, const std::string &, const std::string &, size_t) = &mf::PCRModflowPython::setRiver;
void (mf::PCRModflowPython::*setRiverPy)(const calc::Field *, const calc::Field *, const calc::Field *, size_t) = &mf::PCRModflowPython::setRiver;
void (mf::PCRModflowPython::*setRiverPyBlock)(discr::BlockData<REAL4> &, discr::BlockData<REAL4> &, discr::BlockData<REAL4> &) = &mf::PCRModflowPython::setRiver;

void (mf::PCRModflowPython::*setStoragePy)(const calc::Field *, const calc::Field *, size_t) = &mf::PCRModflowPython::setStorage;
void (mf::PCRModflowPython::*setStoragePS)(const std::string &, const std::string &, size_t) = &mf::PCRModflowPython::setStorage;
void (mf::PCRModflowPython::*setStoragePyBlock)(const discr::BlockData<REAL4> &, const discr::BlockData<REAL4> &) = &mf::PCRModflowPython::setStorage;


bool (mf::PCRModflowPython::*setWellCalc)(const float *, size_t) = &mf::PCRModflowPython::setWell;
void (mf::PCRModflowPython::*setWellPS)(const std::string &, size_t) = &mf::PCRModflowPython::setWell;
void (mf::PCRModflowPython::*setWellPy)(const calc::Field *, size_t) = &mf::PCRModflowPython::setWell;
void (mf::PCRModflowPython::*setWellPyBlock)(discr::BlockData<REAL4> &) = &mf::PCRModflowPython::setWell;

bool (mf::PCRModflowPython::*setDrainCalc)(const float *, const float *, size_t) = &mf::PCRModflowPython::setDrain;
void (mf::PCRModflowPython::*setDrainPS)(const std::string &, const std::string &, size_t) = &mf::PCRModflowPython::setDrain;
void (mf::PCRModflowPython::*setDrainPy)(const calc::Field *, const calc::Field *, size_t) = &mf::PCRModflowPython::setDrain;
void (mf::PCRModflowPython::*setDrainPyBlock)(const discr::BlockData<REAL4> &, const discr::BlockData<REAL4> &) = &mf::PCRModflowPython::setDrain;

void (mf::PCRModflowPython::*setGhbString)(const std::string &, const std::string &, size_t) = &mf::PCRModflowPython::setGHB;
void (mf::PCRModflowPython::*setGhbField)(const calc::Field *, const calc::Field *, size_t) = &mf::PCRModflowPython::setGHB;


void (mf::PCRModflowPython::*getHeads1)(float *, size_t) = &mf::PCRModflowPython::getHeads;
calc::Field* (mf::PCRModflowPython::*getHeads2)(size_t) = &mf::PCRModflowPython::getHeads;

//
void (mf::PCRModflowPython::*getRivLeakCalc)(float *, size_t) = &mf::PCRModflowPython::getRiverLeakage;
calc::Field* (mf::PCRModflowPython::*getRivLeakPy)(size_t) = &mf::PCRModflowPython::getRiverLeakage;

void (mf::PCRModflowPython::*getDrainCalc)(float *, size_t) = &mf::PCRModflowPython::getDrain;
calc::Field* (mf::PCRModflowPython::*getDrainPy)(size_t) = &mf::PCRModflowPython::getDrain;

void (mf::PCRModflowPython::*getRechargeCalc)(float *, size_t) = &mf::PCRModflowPython::getRecharge;
calc::Field* (mf::PCRModflowPython::*getRechargePy)(size_t) = &mf::PCRModflowPython::getRecharge;




void             (mf::PCRModflowPython::*get_storageCalc)       (float *, size_t) = &mf::PCRModflowPython::get_storage;
calc::Field*     (mf::PCRModflowPython::*get_storagePy)         (size_t) = &mf::PCRModflowPython::get_storage;

void             (mf::PCRModflowPython::*get_constand_headCalc) (float *, size_t) = &mf::PCRModflowPython::get_constand_head;
calc::Field*     (mf::PCRModflowPython::*get_constand_headPy)   (size_t) = &mf::PCRModflowPython::get_constand_head;

void             (mf::PCRModflowPython::*get_right_faceCalc)    (float *, size_t) = &mf::PCRModflowPython::get_right_face;
calc::Field*     (mf::PCRModflowPython::*get_right_facePy)      (size_t) = &mf::PCRModflowPython::get_right_face;

void             (mf::PCRModflowPython::*get_front_faceCalc)    (float *, size_t) = &mf::PCRModflowPython::get_front_face;
calc::Field*     (mf::PCRModflowPython::*get_front_facePy)      (size_t) = &mf::PCRModflowPython::get_front_face;

void             (mf::PCRModflowPython::*get_lower_faceCalc)    (float *, size_t) = &mf::PCRModflowPython::get_lower_face;
calc::Field*     (mf::PCRModflowPython::*get_lower_facePy)      (size_t) = &mf::PCRModflowPython::get_lower_face;




void (mf::PCRModflowPython::*setCondPS)(size_t, const std::string &, const std::string &, size_t, bool) = &mf::PCRModflowPython::setCond;



void (mf::PCRModflowPython::*setCondPy)(size_t, const calc::Field *, const calc::Field *, size_t, bool) = &mf::PCRModflowPython::setCond;



PYBIND11_MODULE(_pcraster_modflow, module){
  // disables the C++ signatures in docstrings
  pybind11::options options;
  options.disable_function_signatures();

  // Desired methods in module documentation
  // are enforced by empty docstrings. Ugly, but fttb...


  pybind11::class_<mf::PCRModflowPython>(module, "initialise")
    .def(pybind11::init<geo::RasterSpace const&>())
    .def("run", &mf::PCRModflowPython::runModflow, pybind11::arg("working_directory")="", R"(

    )")
    .def("converged", &mf::PCRModflowPython::converged, R"(

    )")
    .def("_set_run_command", &mf::PCRModflowPython::set_run_command)
    // DIS
    .def("setLayer", &mf::PCRModflowPython::setLayer, R"(

    )")
    .def("createBottomLayer", createBottomPy, R"(

    )")
    .def("createBottomLayer", createBottomPS)
    .def("addLayer", addLayerPS)
    .def("addLayer", addLayerPy, R"(

    )")
    .def("addConfinedLayer", addConfinedPy, R"(

    )")
    .def("addConfinedLayer", addConfinedPS)
    .def("setDISParameter", &mf::PCRModflowPython::setDISParams, R"(

    )")
    .def("setRowWidth", &mf::PCRModflowPython::set_row_width, R"(

    )")
    .def("setColumnWidth", &mf::PCRModflowPython::set_col_width, R"(

    )")
    .def("updateDISParameter", &mf::PCRModflowPython::update_dis_parameter, R"(

    )")
    // BAS
    .def("setBoundary", setBoundaryPy, R"(

    )")
    .def("setBoundary", setBoundaryPS)
    .def("setBoundary", setBoundaryPyBlock)
    .def("setInitialHead", setHeadPS)
    .def("setInitialHead", setHeadPy, R"(

    )")
    .def("setInitialHead", setHeadPyBlock)
    .def("setNoFlowHead", &mf::PCRModflowPython::setNoFlowConstant, R"(

    )")
    // BCF
    //.def("setHorizontalConductivity", setHCond)
    //.def("setVerticalConductivity", setVCond)
    .def("setConductivity", setCondPy,
         "laycon"_a,
         "hcond"_a,
         "vcond"_a,
         "layer"_a,
         "calc"_a=true, /*pybind11::arg("laycon"),pybind11::arg("hcond"),pybind11::arg("vcond"),pybind11::arg("layer"),*/ /*pybind11::arg("calc")=true,*/ R"(

    )")
    .def("setConductivity", setCondPS,
         "laycon"_a,
         "hcond"_a,
         "vcond"_a,
         "layer"_a,
         "calc"_a=true
    )
    .def("setDryHead", &mf::PCRModflowPython::setHDRY, R"(

    )")
    .def("setHorizontalAnisotropy", &mf::PCRModflowPython::setTRPY, R"(

    )")
    .def("setStorage", setStoragePS)
    .def("setStorage", setStoragePy, R"(

    )")
    .def("setStorage", setStoragePyBlock)
    .def("setWetting", setWettingPy, R"(

    )")
    .def("setWetting", setWettingPS)
    .def("setWetting", setWettingPyBlock)
    .def("setWettingParameter", &mf::PCRModflowPython::setWettingParameter, R"(

    )")
    // RIV
    .def("setRiver", setRiverPy, R"(

    )")
    .def("setRiver", setRiverPS)
    .def("setRiver", setRiverPyBlock)
    // RCH
    .def("setRecharge", setRecharge, R"(

    )")
    .def("setRecharge", setRechargePS)
    .def("setIndicatedRecharge", setRechargeLay, R"(

    )")
    .def("setIndicatedRecharge", setRechargeLayPS)
    // WEL
    .def("setWell", setWellPy, R"(

    )")
    .def("setWell", setWellPS)
    .def("setWell", setWellPyBlock)
    // DRN
    .def("setDrain", setDrainPy, R"(

    )")
    .def("setDrain", setDrainPS)
    .def("setDrain", setDrainPyBlock)
    // GHB
    .def("setGeneralHead", setGhbField, R"(

    )")
    .def("setGeneralHead", setGhbString, R"(

    )")
    .def("getGeneralHeadLeakage", &mf::PCRModflowPython::getGHBLeakage, R"(

    )")
    // Solver
    .def("setPCG", &mf::PCRModflowPython::setPCG, R"(

    )")
    .def("setSIP", &mf::PCRModflowPython::setSIP, R"(

    )")
    .def("setSOR", &mf::PCRModflowPython::setSOR, R"(

    )")
    .def("setDSP", &mf::PCRModflowPython::setDSP, R"(

    )")
    //
    //.def("getHeads", &mf::PCRModflowPython::getBlockHeads)
    .def("getHeads", getHeads2, R"(

    )")
   // .def("getRiverLeakage", &mf::PCRModflowPython::getBlockRiverLeakage,
  //       boost::python::return_value_policy<boost::python::manage_new_object>())
    .def("getRiverLeakage", getRivLeakPy, R"(

    )")
    //.def("getDrain", &mf::PCRModflowPython::getBlockDrain,
    //     boost::python::return_value_policy<boost::python::manage_new_object>())
    .def("getDrain", getDrainPy, R"(

    )")
    .def("getRecharge", getRechargeCalc)
    .def("getRecharge", getRechargePy, R"(

    )")
    .def("getStorage", get_storagePy, R"(

    )")
    .def("getConstantHead", get_constand_headPy, R"(

    )")
    .def("getRightFace", get_right_facePy, R"(

    )")
    .def("getFrontFace", get_front_facePy, R"(

    )")
    .def("getLowerFace", get_lower_facePy, R"(

    )")
    ;
}
