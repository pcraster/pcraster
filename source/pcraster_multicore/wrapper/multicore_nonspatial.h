#pragma once

#include <cstddef>

#include "calc_spatial.h"
#include "com_mvop.h"


namespace multicore_field {


template<class T>
class Nonspatial {

public:

                   Nonspatial          (calc::Field* field);

                   ~Nonspatial         (){};

  T&               get();

  T const&         get() const;

  T*               get_cells() const;

  void             set_mv() const;

  calc::Field*     getField() const;

protected:

private:

  calc::Field*     pcr_field;

  T*               the_cells;

};


template<class T>
Nonspatial<T>::Nonspatial(calc::Field* field){
  pcr_field = field;
  the_cells = static_cast<T*>(field->dest());
}


template<class T>
inline calc::Field* Nonspatial<T>::getField() const {
  return pcr_field;
}


template<class T>
inline T const & Nonspatial<T>::get() const {
  return the_cells[0];
}


template<class T>
inline T& Nonspatial<T>::get(){
  return the_cells[0];
}


template<class T>
inline void Nonspatial<T>::set_mv() const {
  pcr::setMV(the_cells[0]);
}


template<class T>
inline T* Nonspatial<T>::get_cells() const {
  return the_cells;
}


} // namespace multicore_field
