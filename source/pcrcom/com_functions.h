#ifndef INCLUDED_COM_FUNCTIONS
#define INCLUDED_COM_FUNCTIONS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif



namespace com {
  // Functions declarations.
}



namespace com {



template<class InputIterator>
typename std::iterator_traits<InputIterator>::value_type minimum(
         InputIterator from_pos, InputIterator from_end) {

  typedef typename std::iterator_traits<InputIterator>::value_type value_type;
  value_type value = value_type();
  pcr::setMV(value);

  // Determine initial value.
  while(from_pos != from_end) {

    if(!pcr::isMV(*from_pos)) {
      value = *from_pos;
      break;
    }

    ++from_pos;
  }

  // Determine minimum value.
  while(from_pos != from_end) {

    if(!pcr::isMV(*from_pos)) {
      value = MIN(value, *from_pos);
    }

    ++from_pos;
  }

  return value;
}



template<class InputIterator>
typename std::iterator_traits<InputIterator>::value_type maximum(
         InputIterator from_pos, InputIterator from_end) {

  typedef typename std::iterator_traits<InputIterator>::value_type value_type;
  value_type value = value_type();
  pcr::setMV(value);

  // Determine initial value.
  while(from_pos != from_end) {

    if(!pcr::isMV(*from_pos)) {
      value = *from_pos;
      break;
    }

    ++from_pos;
  }

  // Determine maximum value.
  while(from_pos != from_end) {

    if(!pcr::isMV(*from_pos)) {
      value = MAX(value, *from_pos);
    }

    ++from_pos;
  }

  return value;
}



template<class InputIterator>
typename std::iterator_traits<InputIterator>::value_type sum(
         InputIterator from_pos, InputIterator from_end) {

  typename std::iterator_traits<InputIterator>::value_type value;
  pcr::setMV(value);

  // Determine initial value.
  while(from_pos != from_end) {

    if(!pcr::isMV(*from_pos)) {
      value = *from_pos;
      break;
    }

    ++from_pos;
  }

  // Determine minimum value.
  while(from_pos != from_end) {

    if(!pcr::isMV(*from_pos)) {
      value += *from_pos;
    }

    ++from_pos;
  }

  return value;
}



template<class InputIterator>
double average(InputIterator from_pos, InputIterator from_end) {

  size_t nrValues = 0;
  double value = 0.0;

  while(from_pos != from_end) {

    if(!pcr::isMV(*from_pos)) {
      value += *from_pos;
      ++nrValues;
    }

    ++from_pos;
  }

  if(nrValues) {
    value /= nrValues;
  }
  else {
    pcr::setMV(value);
  }

  return value;
}





} // namespace com

#endif
