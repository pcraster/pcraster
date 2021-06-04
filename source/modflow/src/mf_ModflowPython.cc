#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MF_MODFLOW
#include "mf_ModflowPython.h"
#define INCLUDED_MF_MODFLOW
#endif

#include "dis.h"

#include <pybind11/pybind11.h>

#include <sstream>

/*!
  \file
  This file contains the implementation of the Modflow class.
*/



namespace mf {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MODFLOW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MODFLOW MEMBERS
//------------------------------------------------------------------------------

PCRModflowPython::PCRModflowPython(size_t rows, size_t cols, double cellsize, double xll, double yll)
    : PCRModflow(rows, cols, cellsize, xll, yll)
{
}

PCRModflowPython::PCRModflowPython(const geo::RasterSpace &raster) : PCRModflow (raster)
{
}

PCRModflowPython::~PCRModflowPython()
{
}


void PCRModflowPython::set_row_width(pybind11::list const& arguments){

  if(d_dis == nullptr){
    throw std::invalid_argument("Error in PCRasterModflow setRowWidth: Layers need to be specified at first!");
  }

  size_t nr_args = pybind11::len(arguments);

  if(nr_args != d_nrOfRows){
    std::ostringstream errMsg;
    errMsg << "Error in PCRasterModflow setRowWidth:";
    errMsg << nr_args << " row widths given while " << d_nrOfRows << " are required";
    throw std::invalid_argument(errMsg.str());
  }

  d_dis->reset_row_width();

  for(size_t idx = 0; idx < nr_args; ++idx){
    d_dis->append_row_width(arguments[idx].cast<float>());
  }

}


void PCRModflowPython::set_col_width(pybind11::list const& arguments){

  if(d_dis == nullptr){
    throw std::invalid_argument("Error in PCRasterModflow setColumnWidth: Layers need to be specified at first!");
  }

  size_t nr_args = pybind11::len(arguments);

  if(nr_args != d_nrOfColumns){
    std::ostringstream errMsg;
    errMsg << "Error in PCRasterModflow setColumnWidth:";
    errMsg << nr_args << " column widths given while " << d_nrOfColumns << " are required";
    throw std::invalid_argument(errMsg.str());
  }

  d_dis->reset_col_width();

  for(size_t idx = 0; idx < nr_args; ++idx){
    d_dis->append_col_width(arguments[idx].cast<float>());
  }

}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace mf

