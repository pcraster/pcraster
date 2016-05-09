#pragma once

#include "pcraster_multicore/wrapper/datatype_traits/multicore_nonspatial.h"
#include "pcraster_multicore/wrapper/multicore_nonspatial.h"

namespace fern {


template<class T>
inline size_t size(
    multicore_field::Nonspatial<T> const& /*aField*/){

  return 1;
}


template<class T>
inline T const& get(
    multicore_field::Nonspatial<T> const& aField){

  return aField.get();
}


template<class T>
inline T& get(
    multicore_field::Nonspatial<T>& aField){

  return aField.get();
}



} // namespace fern
