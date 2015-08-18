#ifndef INCLUDED_COM_BINARYOPERATORS
#define INCLUDED_COM_BINARYOPERATORS



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
  // BinaryOperators declarations.
}



namespace com {

template<class ValueType, class OutputIterator>
OutputIterator multiplyByValue(OutputIterator to_pos, OutputIterator to_end,
         ValueType from) {

  if(pcr::isMV(from)) {
    while(to_pos != to_end) {
      pcr::setMV(*to_pos);
      ++to_pos;
    }
  }
  else {
    while(to_pos != to_end) {
      if(!pcr::isMV(*to_pos)) {
        *to_pos *= from;
      }
      ++to_pos;
    }
  }

  return to_pos;
}

template<class InputIterator, class OutputIterator>
OutputIterator multiplyByRange(OutputIterator to_pos, OutputIterator to_end,
         InputIterator from_pos) {

  while(to_pos != to_end) {
    if(!pcr::isMV(*to_pos)) {
      if(pcr::isMV(*from_pos)) {
        pcr::setMV(*to_pos);
      }
      else {
        *to_pos *= *from_pos;
      }
    }
    ++from_pos;
    ++to_pos;
  }

  return to_pos;
}

template<class ValueType, class OutputIterator>
OutputIterator divideByValue(OutputIterator to_pos, OutputIterator to_end,
         ValueType from) {

  if(pcr::isMV(from) || from == ValueType(0)) {
    while(to_pos != to_end) {
      pcr::setMV(*to_pos);
      ++to_pos;
    }
  }
  else {
    while(to_pos != to_end) {
      if(!pcr::isMV(*to_pos)) {
        *to_pos /= from;
      }
      ++to_pos;
    }
  }

  return to_pos;
}

template<class InputIterator, class OutputIterator>
OutputIterator divideByRange(OutputIterator to_pos, OutputIterator to_end,
         InputIterator from_pos) {

  typename std::iterator_traits<InputIterator>::value_type zero(0);

  while(to_pos != to_end) {
    if(!pcr::isMV(*to_pos)) {
      if(pcr::isMV(*from_pos) || *from_pos == zero) {
        pcr::setMV(*to_pos);
      }
      else {
        *to_pos /= *from_pos;
      }
    }
    ++from_pos;
    ++to_pos;
  }

  return to_pos;
}

template<class ValueType, class OutputIterator>
OutputIterator addValue(OutputIterator to_pos, OutputIterator to_end,
         ValueType from) {

  if(pcr::isMV(from)) {
    while(to_pos != to_end) {
      pcr::setMV(*to_pos);
      ++to_pos;
    }
  }
  else {
    while(to_pos != to_end) {
      if(!pcr::isMV(*to_pos)) {
        *to_pos += from;
      }
      ++to_pos;
    }
  }

  return to_pos;
}

template<class InputIterator, class OutputIterator>
OutputIterator addRange(OutputIterator to_pos, OutputIterator to_end,
         InputIterator from_pos) {

  while(to_pos != to_end) {
    if(!pcr::isMV(*to_pos)) {
      if(pcr::isMV(*from_pos)) {
        pcr::setMV(*to_pos);
      }
      else {
        *to_pos += *from_pos;
      }
    }
    ++from_pos;
    ++to_pos;
  }

  return to_pos;
}

template<class ValueType, class OutputIterator>
OutputIterator substractValue(OutputIterator to_pos, OutputIterator to_end,
         ValueType from) {

  if(pcr::isMV(from)) {
    while(to_pos != to_end) {
      pcr::setMV(*to_pos);
      ++to_pos;
    }
  }
  else {
    while(to_pos != to_end) {
      if(!pcr::isMV(*to_pos)) {
        *to_pos -= from;
      }
      ++to_pos;
    }
  }

  return to_pos;
}

template<class InputIterator, class OutputIterator>
OutputIterator substractRange(OutputIterator to_pos, OutputIterator to_end,
         InputIterator from_pos) {

  while(to_pos != to_end) {
    if(!pcr::isMV(*to_pos)) {
      if(pcr::isMV(*from_pos)) {
        pcr::setMV(*to_pos);
      }
      else {
        *to_pos -= *from_pos;
      }
    }
    ++from_pos;
    ++to_pos;
  }

  return to_pos;
}

} // namespace com

#endif
