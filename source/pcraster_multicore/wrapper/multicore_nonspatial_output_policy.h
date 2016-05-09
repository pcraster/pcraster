#pragma once

#include "csftypes.h"

#include "pcraster_multicore/wrapper/multicore_nonspatial.h"
#include "fern/core/data_type_traits/scalar.h"

template<class T>
class NonspatialSetNoData
{

private:

    using value_type = typename fern::DataTypeTraits<T>::value_type;

public:

                   NonspatialSetNoData (multicore_field::Nonspatial<T> & aField);

                   ~NonspatialSetNoData()=default;

    void           mark_as_no_data     ();

protected:

                   NonspatialSetNoData ()=delete;

                   NonspatialSetNoData (NonspatialSetNoData const&)=delete;

                   NonspatialSetNoData (NonspatialSetNoData&&)=default;

    NonspatialSetNoData&    operator=  (NonspatialSetNoData const&)=delete;

    NonspatialSetNoData&    operator=  (NonspatialSetNoData&&)=default;

private:

    multicore_field::Nonspatial<T> & _field;

};


template<class T>
inline NonspatialSetNoData<T>::NonspatialSetNoData(
    multicore_field::Nonspatial<T> & aField)

    : _field(aField)

{
}


template<class T>
inline void NonspatialSetNoData<T>::mark_as_no_data(){
   _field.set_mv();
}
