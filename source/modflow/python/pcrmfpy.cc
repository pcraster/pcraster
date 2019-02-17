#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif


// PCRaster library headers.
#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif
#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif

#include "mf_ModflowPython.h"
#include <pybind11/pybind11.h>


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




void (mf::PCRModflowPython::*setCondPS)(size_t, const std::string &, const std::string &, size_t) = &mf::PCRModflowPython::setCond;



void (mf::PCRModflowPython::*setCondPy)(size_t, const calc::Field *, const calc::Field *, size_t) = &mf::PCRModflowPython::setCond;



PYBIND11_MODULE(_pcraster_modflow, module){

  pybind11::class_<mf::PCRModflowPython>(module, "initialise")
    .def(pybind11::init<geo::RasterSpace const&>())
    .def("run", &mf::PCRModflowPython::runModflow, pybind11::arg("working_directory")="")
    .def("converged", &mf::PCRModflowPython::converged)
    .def("_set_run_command", &mf::PCRModflowPython::set_run_command)
    // DIS
    .def("setLayer", &mf::PCRModflowPython::setLayer)
    .def("createBottomLayer", createBottomPy)
    .def("createBottomLayer", createBottomPS)
    .def("addLayer", addLayerPS)
    .def("addLayer", addLayerPy)
    .def("addConfinedLayer", addConfinedPy)
    .def("addConfinedLayer", addConfinedPS)
    .def("setDISParameter", &mf::PCRModflowPython::setDISParams)
    .def("setRowWidth", &mf::PCRModflowPython::set_row_width)
    .def("setColumnWidth", &mf::PCRModflowPython::set_col_width)
    .def("updateDISParameter", &mf::PCRModflowPython::update_dis_parameter)
    // BAS
    .def("setBoundary", setBoundaryPy)
    .def("setBoundary", setBoundaryPS)
    .def("setBoundary", setBoundaryPyBlock)
    .def("setInitialHead", setHeadPS)
    .def("setInitialHead", setHeadPy)
    .def("setInitialHead", setHeadPyBlock)
    .def("setNoFlowHead", &mf::PCRModflowPython::setNoFlowConstant)
    // BCF
    //.def("setHorizontalConductivity", setHCond)
    //.def("setVerticalConductivity", setVCond)
    .def("setConductivity", setCondPy)
    .def("setConductivity", setCondPS)
    .def("setDryHead", &mf::PCRModflowPython::setHDRY)
    .def("setHorizontalAnisotropy", &mf::PCRModflowPython::setTRPY)
    .def("setStorage", setStoragePS)
    .def("setStorage", setStoragePy)
    .def("setStorage", setStoragePyBlock)
    .def("setWetting", setWettingPy)
    .def("setWetting", setWettingPS)
    .def("setWetting", setWettingPyBlock)
    .def("setWettingParameter", &mf::PCRModflowPython::setWettingParameter)
    // RIV
    .def("setRiver", setRiverPy)
    .def("setRiver", setRiverPS)
    .def("setRiver", setRiverPyBlock)
    // RCH
    .def("setRecharge", setRecharge)
    .def("setRecharge", setRechargePS)
    .def("setIndicatedRecharge", setRechargeLay)
    .def("setIndicatedRecharge", setRechargeLayPS)
    // WEL
    .def("setWell", setWellPy)
    .def("setWell", setWellPS)
    .def("setWell", setWellPyBlock)
    // DRN
    .def("setDrain", setDrainPy)
    .def("setDrain", setDrainPS)
    .def("setDrain", setDrainPyBlock)
    // Solver
    .def("setPCG", &mf::PCRModflowPython::setPCG)
    .def("setSIP", &mf::PCRModflowPython::setSIP)
    .def("setSOR", &mf::PCRModflowPython::setSOR)
    .def("setDSP", &mf::PCRModflowPython::setDSP)
    //
    //.def("getHeads", &mf::PCRModflowPython::getBlockHeads)
    .def("getHeads", getHeads2)
   // .def("getRiverLeakage", &mf::PCRModflowPython::getBlockRiverLeakage,
  //       boost::python::return_value_policy<boost::python::manage_new_object>())
    .def("getRiverLeakage", getRivLeakPy)
    //.def("getDrain", &mf::PCRModflowPython::getBlockDrain,
    //     boost::python::return_value_policy<boost::python::manage_new_object>())
    .def("getDrain", getDrainPy)
    .def("getRecharge", getRechargeCalc)
    .def("getRecharge", getRechargePy)
    .def("getStorage", get_storagePy)
    .def("getConstantHead", get_constand_headPy)
    .def("getRightFace", get_right_facePy)
    .def("getFrontFace", get_front_facePy)
    .def("getLowerFace", get_lower_facePy)
    ;

}
