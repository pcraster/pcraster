#pragma once

#include "fern/core/data_type_traits.h"
#include "pcraster_multicore/wrapper/multicore_nonspatial.h"

namespace fern {


template<class T>
struct DataTypeTraits<multicore_field::Nonspatial<T>>{

  using value_type = T;

  using reference = T&;

  using const_reference = T const&;

  using argument_category = scalar_tag;

  template<class U>
  struct Clone{
    using type = multicore_field::Nonspatial<U>;
  };

};

} // namespace fern
