#include "pcraster_multicore/python/type_conversion/ordinal.h"

#include <cstdlib>

// PCRaster
#include "calc_spatial.h"
#include "calc_nonspatial.h"
#include "pcrtypes.h"

// Field wrapper
#include "pcraster_multicore/python/execution_policy.h"
#include "pcraster_multicore/python/local/utils.h"

// Fern
#include "fern/core/thread_pool.h"



namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {



template<class T>
void ordinal_nonspatial(double cell_value, calc::Field * res_cells){

  double res;
  if(!pcr::isMV(cell_value)) {
    res = cell_value != static_cast<T>(0) ? 1 : 0;
  }
  else{
    pcr::setMV(res);
  }
  res_cells->setCell(res, 0);
}



template<class T>
void to_ordinal(const T* in_cells, INT4* res_cells, size_t start, size_t end){

  INT4 res;
  T cell_value = static_cast<T>(0);

  for (size_t c = start; c < end; ++c){
    cell_value = in_cells[c];
    if(!pcr::isMV(cell_value)) {
      res = static_cast<INT4>(cell_value);
    }
    else{
      pcr::setMV(res);
    }
    res_cells[c] = res;
  }
}

template<class T>
void ordinal_spatial(const T* in_cells, INT4* res_cells){

  size_t nr_threads = nr_cpus();
  fern::ThreadPool pool(nr_threads);

  std::vector<std::future<void>> futures;
  futures.reserve(nr_threads);

  auto arg = std::cref(in_cells);
  auto res = std::ref(res_cells);

  std::lldiv_t interval = std::div(static_cast<long long>(nr_cells()), static_cast<long long>(nr_threads));
  size_t segment_size = interval.quot;

  for(size_t segments = 0; segments < nr_threads; ++segments){
    size_t segment_start = segments * segment_size;
    size_t segment_end = segment_start + segment_size;
    if(segments == nr_threads - 1){
      segment_end = nr_cells();
    }
    auto function = std::bind(to_ordinal<T>, arg, res, segment_start, segment_end);
    futures.emplace_back(pool.submit(function));
  }

  for(auto& future: futures){
    future.get();
  }
}


} // namespace detail


calc::Field* ordinal(
         calc::Field* field){

  assert_equal_location_attributes(*field);

  if(ordinal_valuescale(*field)){
    calc::Field* res_field = field->createClone();
    return res_field;
  }

  if(directional_valuescale(*field)){
    throw std::runtime_error("operation not implemented for type 'directional'");
  }

  calc::Field* res_field = nullptr;

  CSF_CR cell_representation = field->cr();

  if(field->isSpatial() == false){
    res_field = new calc::NonSpatial(VS_O);
    double in_value = 0.0;
    field->getCell(in_value, 0);

    switch(cell_representation) {
      case CR_UINT1:{
        detail::ordinal_nonspatial<UINT1>(in_value, res_field);
        break;
      }
      case CR_INT4:{
        detail::ordinal_nonspatial<INT4>(in_value, res_field);
        break;
      }
      case CR_REAL4:{
        detail::ordinal_nonspatial<REAL4>(in_value, res_field);
        break;
      }
      default: {
        throw std::runtime_error("internal error: unable to perform operation");
      }
    }
    return res_field;
  }

  res_field = new calc::Spatial(VS_O, calc::CRI_4, nr_cells());
  INT4* res_cells = res_field->dest_4();

  switch(cell_representation) {
    case CR_UINT1:{
      const UINT1* in_cells = field->src_1();
      detail::ordinal_spatial<UINT1>(in_cells, res_cells);
      break;
    }
    case CR_INT4:{
      const INT4* in_cells = field->src_4();
      detail::ordinal_spatial<INT4>(in_cells, res_cells);
      break;
    }
    case CR_REAL4:{
      const REAL4* in_cells = field->src_f();
      detail::ordinal_spatial<REAL4>(in_cells, res_cells);
      break;
    }
    default: {
      throw std::runtime_error("internal error: unable to perform operation");
      break;
    }
  }

  return res_field;
}


} // namespace python
} // namespace pcraster_multicore

