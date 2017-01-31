#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_BOOST_PYTHON
#include <boost/python.hpp>
#define INCLUDED_BOOST_PYTHON
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

#ifndef INCLUDED_PCRMODFLOW
#include "pcrmodflow.h"
#define INCLUDED_PCRMODFLOW
#endif


// Module headers.

#if _MSC_VER == 1900
  // Workaround wrt Boost Python and VS2015v3
  namespace boost
  {
    template <>
    calc::Field const volatile * get_pointer(class calc::Field const volatile *f)
    {
        return f;
    }
  }
#endif






// Wrapper for overloaded methods
bool (PCRModflow::*createBottomCalc)(const float *, const float *) = &PCRModflow::createBottom;
void (PCRModflow::*createBottomPy)(const calc::Field *, const calc::Field *) = &PCRModflow::createBottom;
void (PCRModflow::*createBottomPS)(const std::string&, const std::string&) = &PCRModflow::createBottomPS;
//void (PCRModflow::*setLayer)(const discr::Block &, const discr::BlockData<INT4> &) = &PCRModflow::setLayer;
bool (PCRModflow::*addLayerCalc)(const float *) = &PCRModflow::addLayer;
void (PCRModflow::*addLayerPy)(const calc::Field *) = &PCRModflow::addLayer;
void (PCRModflow::*addLayerPS)(const std::string&) = &PCRModflow::addLayerPS;

bool (PCRModflow::*addConfinedCalc)(const float *) = &PCRModflow::addConfinedLayer;
void (PCRModflow::*addConfinedPy)(const calc::Field *) = &PCRModflow::addConfinedLayer;
void (PCRModflow::*addConfinedPS)(const std::string&) = &PCRModflow::addConfinedLayerPS;

bool (PCRModflow::*setBoundaryCalc)(const int *, size_t) = &PCRModflow::setIBound;
void (PCRModflow::*setBoundaryPS)(const std::string&, size_t) = &PCRModflow::setIBound;
void (PCRModflow::*setBoundaryPy)(const calc::Field *values, size_t layer) = &PCRModflow::setIBound;
bool (PCRModflow::*setBoundaryPyBlock)(const discr::BlockData<INT4> &) = &PCRModflow::setIBound;

bool (PCRModflow::*setHeadCalc)(const float *, size_t) = &PCRModflow::setInitialHead;
void (PCRModflow::*setHeadPS)(const std::string &, size_t) = &PCRModflow::setInitialHead;
void (PCRModflow::*setHeadPy)(const calc::Field *, size_t) = &PCRModflow::setInitialHead;
bool (PCRModflow::*setHeadPyBlock)(const discr::BlockData<REAL4> &) = &PCRModflow::setInitialHead;


bool (PCRModflow::*setHCond2)(const float *, size_t, size_t) = &PCRModflow::setHCond;
void (PCRModflow::*setHCond)(const discr::BlockData<REAL4> &, const discr::BlockData<INT4> &) = &PCRModflow::setHCond;

//bool (PCRModflow::*setVCond1)(const std::string &, size_t) = &PCRModflow::setVCond;
bool (PCRModflow::*setVCond2)(const float *, size_t) = &PCRModflow::setVCond;
void (PCRModflow::*setVCond)(const discr::BlockData<REAL4> &) = &PCRModflow::setVCond;

bool (PCRModflow::*setWettingCalc)(const float *, size_t) = &PCRModflow::setWetting;
void (PCRModflow::*setWettingPS)(const std::string &, size_t) = &PCRModflow::setWetting;
void (PCRModflow::*setWettingPy)(const calc::Field *, size_t) = &PCRModflow::setWetting;
void (PCRModflow::*setWettingPyBlock)(const discr::BlockData<REAL4> &) = &PCRModflow::setWetting;



//bool (PCRModflow::*setRiv1)(const std::string &, const std::string &, const std::string &, size_t) = &PCRModflow::setRiver;
bool (PCRModflow::*setRiv2)(const float *, const float *, const float *, size_t) = &PCRModflow::setRiver;
void (PCRModflow::*setRiv)(discr::BlockData<REAL4> &, discr::BlockData<REAL4> &, discr::BlockData<REAL4> &) = &PCRModflow::setRiver;

void (PCRModflow::*setRecharge1)(const float *, size_t) = &PCRModflow::setRecharge;
void (PCRModflow::*setRechargeLay1)(const float *, const int *) = &PCRModflow::setRechargeLay;
void (PCRModflow::*setRecharge)(const calc::Field *, size_t) = &PCRModflow::setRecharge;
void (PCRModflow::*setRechargeLay)(const calc::Field *, const calc::Field *) = &PCRModflow::setRechargeLay;
void (PCRModflow::*setRechargePS)(const std::string &, size_t) = &PCRModflow::setRecharge;
void (PCRModflow::*setRechargeLayPS)(const std::string &, const std::string &) = &PCRModflow::setRechargeLay;

bool (PCRModflow::*setRiverCalc)(const float *, const float *, const float *, size_t) = &PCRModflow::setRiver;
void (PCRModflow::*setRiverPS)(const std::string &, const std::string &, const std::string &, size_t) = &PCRModflow::setRiver;
void (PCRModflow::*setRiverPy)(const calc::Field *, const calc::Field *, const calc::Field *, size_t) = &PCRModflow::setRiver;
void (PCRModflow::*setRiverPyBlock)(discr::BlockData<REAL4> &, discr::BlockData<REAL4> &, discr::BlockData<REAL4> &) = &PCRModflow::setRiver;

void (PCRModflow::*setStoragePy)(const calc::Field *, const calc::Field *, size_t) = &PCRModflow::setStorage;
void (PCRModflow::*setStoragePS)(const std::string &, const std::string &, size_t) = &PCRModflow::setStorage;
void (PCRModflow::*setStoragePyBlock)(const discr::BlockData<REAL4> &, const discr::BlockData<REAL4> &) = &PCRModflow::setStorage;


bool (PCRModflow::*setWellCalc)(const float *, size_t) = &PCRModflow::setWell;
void (PCRModflow::*setWellPS)(const std::string &, size_t) = &PCRModflow::setWell;
void (PCRModflow::*setWellPy)(const calc::Field *, size_t) = &PCRModflow::setWell;
void (PCRModflow::*setWellPyBlock)(discr::BlockData<REAL4> &) = &PCRModflow::setWell;

bool (PCRModflow::*setDrainCalc)(const float *, const float *, size_t) = &PCRModflow::setDrain;
void (PCRModflow::*setDrainPS)(const std::string &, const std::string &, size_t) = &PCRModflow::setDrain;
void (PCRModflow::*setDrainPy)(const calc::Field *, const calc::Field *, size_t) = &PCRModflow::setDrain;
void (PCRModflow::*setDrainPyBlock)(const discr::BlockData<REAL4> &, const discr::BlockData<REAL4> &) = &PCRModflow::setDrain;


void (PCRModflow::*getHeads1)(float *, size_t) = &PCRModflow::getHeads;
calc::Field* (PCRModflow::*getHeads2)(size_t) = &PCRModflow::getHeads;

//
void (PCRModflow::*getRivLeakCalc)(float *, size_t) = &PCRModflow::getRiverLeakage;
calc::Field* (PCRModflow::*getRivLeakPy)(size_t) = &PCRModflow::getRiverLeakage;

void (PCRModflow::*getDrainCalc)(float *, size_t) = &PCRModflow::getDrain;
calc::Field* (PCRModflow::*getDrainPy)(size_t) = &PCRModflow::getDrain;

void (PCRModflow::*getRechargeCalc)(float *, size_t) = &PCRModflow::getRecharge;
calc::Field* (PCRModflow::*getRechargePy)(size_t) = &PCRModflow::getRecharge;


void             (PCRModflow::*get_storageCalc)       (float *, size_t) = &PCRModflow::get_storage;
calc::Field*     (PCRModflow::*get_storagePy)         (size_t) = &PCRModflow::get_storage;

void             (PCRModflow::*get_constand_headCalc) (float *, size_t) = &PCRModflow::get_constand_head;
calc::Field*     (PCRModflow::*get_constand_headPy)   (size_t) = &PCRModflow::get_constand_head;

void             (PCRModflow::*get_right_faceCalc)    (float *, size_t) = &PCRModflow::get_right_face;
calc::Field*     (PCRModflow::*get_right_facePy)      (size_t) = &PCRModflow::get_right_face;

void             (PCRModflow::*get_front_faceCalc)    (float *, size_t) = &PCRModflow::get_front_face;
calc::Field*     (PCRModflow::*get_front_facePy)      (size_t) = &PCRModflow::get_front_face;

void             (PCRModflow::*get_lower_faceCalc)    (float *, size_t) = &PCRModflow::get_lower_face;
calc::Field*     (PCRModflow::*get_lower_facePy)      (size_t) = &PCRModflow::get_lower_face;




void (PCRModflow::*setCondPS)(size_t, const std::string &, const std::string &, size_t) = &PCRModflow::setCond;



void (PCRModflow::*setCondPy)(size_t, const calc::Field *, const calc::Field *, size_t) = &PCRModflow::setCond;


BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(
    run_cwd, PCRModflow::runModflow, 0, 0)
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(
    run_subdirectory, PCRModflow::runModflow, 1, 1)


BOOST_PYTHON_MODULE(_pcraster_modflow){

  boost::python::class_<PCRModflow,boost::noncopyable>("initialise", boost::python::init<const geo::RasterSpace &>())
    //.def("run", &PCRModflow::runModflow)
    .def("run",&PCRModflow::runModflow, run_cwd())
    .def("run",&PCRModflow::runModflow, run_subdirectory())
    .def("converged", &PCRModflow::converged)
    .def("_set_run_command", &PCRModflow::set_run_command)
    // DIS
    .def("setLayer", &PCRModflow::setLayer)
    .def("createBottomLayer", createBottomPy)
    .def("createBottomLayer", createBottomPS)
    .def("addLayer", addLayerPS)
    .def("addLayer", addLayerPy)
    .def("addConfinedLayer", addConfinedPy)
    .def("addConfinedLayer", addConfinedPS)
    .def("setDISParameter", &PCRModflow::setDISParams)
    .def("setRowWidth", &PCRModflow::set_row_width)
    .def("setColumnWidth", &PCRModflow::set_col_width)
    .def("updateDISParameter", &PCRModflow::update_dis_parameter)
    // BAS
    .def("setBoundary", setBoundaryPy)
    .def("setBoundary", setBoundaryPS)
    .def("setBoundary", setBoundaryPyBlock)
    .def("setInitialHead", setHeadPS)
    .def("setInitialHead", setHeadPy)
    .def("setInitialHead", setHeadPyBlock)
    .def("setNoFlowHead", &PCRModflow::setNoFlowConstant)
    // BCF
    //.def("setHorizontalConductivity", setHCond)
    //.def("setVerticalConductivity", setVCond)
    .def("setConductivity", setCondPy)
    .def("setConductivity", setCondPS)
    .def("setDryHead", &PCRModflow::setHDRY)
    .def("setHorizontalAnisotropy", &PCRModflow::setTRPY)
    .def("setStorage", setStoragePS)
    .def("setStorage", setStoragePy)
    .def("setStorage", setStoragePyBlock)
    .def("setWetting", setWettingPy)
    .def("setWetting", setWettingPS)
    .def("setWetting", setWettingPyBlock)
    .def("setWettingParameter", &PCRModflow::setWettingParameter)
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
    .def("setPCG", &PCRModflow::setPCG)
    .def("setSIP", &PCRModflow::setSIP)
    .def("setSOR", &PCRModflow::setSOR)
    .def("setDSP", &PCRModflow::setDSP)
    //
    .def("getHeads", &PCRModflow::getBlockHeads,
         boost::python::return_value_policy<boost::python::manage_new_object>())
    .def("getHeads", getHeads2,
         boost::python::return_value_policy<boost::python::manage_new_object>())

   // .def("getRiverLeakage", &PCRModflow::getBlockRiverLeakage,
  //       boost::python::return_value_policy<boost::python::manage_new_object>())
    .def("getRiverLeakage", getRivLeakPy,
         boost::python::return_value_policy<boost::python::manage_new_object>())

    //.def("getDrain", &PCRModflow::getBlockDrain,
    //     boost::python::return_value_policy<boost::python::manage_new_object>())
    .def("getDrain", getDrainPy,
         boost::python::return_value_policy<boost::python::manage_new_object>())

    .def("getRecharge", getRechargeCalc,
         boost::python::return_value_policy<boost::python::manage_new_object>())
    .def("getRecharge", getRechargePy,
         boost::python::return_value_policy<boost::python::manage_new_object>())


    .def("getStorage", get_storagePy,
         boost::python::return_value_policy<boost::python::manage_new_object>())

    .def("getConstantHead", get_constand_headPy,
         boost::python::return_value_policy<boost::python::manage_new_object>())

    .def("getRightFace", get_right_facePy,
         boost::python::return_value_policy<boost::python::manage_new_object>())

    .def("getFrontFace", get_front_facePy,
         boost::python::return_value_policy<boost::python::manage_new_object>())

    .def("getLowerFace", get_lower_facePy,
         boost::python::return_value_policy<boost::python::manage_new_object>())

    ;

}
