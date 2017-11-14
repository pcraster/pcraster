#pragma once

#include <cstddef>

#include "calc_spatial.h"
#include "com_mvop.h"
#include "Globals.h"


namespace multicore_field {


template<
    typename T>
class Spatial {

public:

                   Spatial             (calc::Field* field,
                                        bool const delete_upon_exit=false);

                   ~Spatial            ();

  T&               get(size_t index);

  T const&         get(size_t index) const;

  size_t           index(size_t idx1, size_t idx2) const;

  size_t           size(size_t dimension) const;

  T*               get_cells() const;

  void             set_cell(size_t idx2) const;

  calc::Field*     getField() const;

  double           cell_size() const;

protected:

private:

  calc::Field*     pcr_field;

  bool const       _delete_upon_exit;

  T*               the_cells;

  size_t           rows;

  size_t           cols;

  double           _cell_size;

};


template<class T>
inline Spatial<T>::Spatial(
    calc::Field* field,
    bool delete_upon_exit)

    : pcr_field(field),
      _delete_upon_exit(delete_upon_exit)

{
    the_cells = static_cast<T*>(pcr_field->dest());
    rows = pcraster::python::globals.cloneSpace().nrRows();
    cols = pcraster::python::globals.cloneSpace().nrCols();
    _cell_size = pcraster::python::globals.cloneSpace().cellSize();
}


template<class T>
inline Spatial<T>::~Spatial()
{
    if(_delete_upon_exit) {
        delete pcr_field;
    }
}


template<class T>
inline calc::Field* Spatial<T>::getField() const {
  return pcr_field;
}


template<class T>
inline T const & Spatial<T>::get(size_t index) const {
  return the_cells[index];
}


template<class T>
inline T& Spatial<T>::get(size_t index){
  return the_cells[index];
}


template<class T>
inline size_t Spatial<T>::size(size_t dimension) const {
  if(dimension == 0){
    return rows;
  }
  else if (dimension == 1){
    return cols;
  }
  assert(0); // never should reach this
  return 0;
}


template<class T>
inline void Spatial<T>::set_cell(size_t index) const {
  pcr::setMV(the_cells[index]);
}


template<class T>
inline size_t Spatial<T>::index(size_t idx1, size_t idx2) const {
  return idx1 * cols + idx2;
}


template<class T>
inline T* Spatial<T>::get_cells() const {
  return the_cells;
}


template<class T>
inline double Spatial<T>::cell_size() const {
  return _cell_size;
}

} // namespace multicore_field
