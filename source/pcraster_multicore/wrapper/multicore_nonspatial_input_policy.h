#pragma once

#include "csftypes.h"
#include "com_mvop.h"

#include "multicore_nonspatial.h"
#include "fern/core/data_type_traits/scalar.h"


template<class T>
class NonspatialDetectNoData
{

private:

    using value_type = fern::value_type<T>;


public:

                   NonspatialDetectNoData(multicore_field::Nonspatial<T> const& aField);

                   ~NonspatialDetectNoData()=default;

                   NonspatialDetectNoData(NonspatialDetectNoData const&)=default;

    NonspatialDetectNoData&  operator= (NonspatialDetectNoData const&)=default;

    bool           is_no_data          () const;

    bool           is_no_data          (size_t index) const;  /// todo remove this!!!

protected:

                   NonspatialDetectNoData        ()=delete;

private:

    multicore_field::Nonspatial<T> const&    _field;

};


template<class T>
inline NonspatialDetectNoData<T>::NonspatialDetectNoData(
    multicore_field::Nonspatial<T> const& aField)

    : _field(aField)

{
}


template<class T>
inline bool NonspatialDetectNoData<T>::is_no_data() const {
  return pcr::isMV(_field.get_cells()[0]);
}


template<class T>
inline bool NonspatialDetectNoData<T>::is_no_data(size_t /* index */) const {
  // this method should not be necessary
  // remove this with newer version of fern supporting the () in 0d_2d ops
  return pcr::isMV(_field.get_cells()[0]);
}

