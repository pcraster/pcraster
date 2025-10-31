#ifndef INCLUDED_COM_DEBUG
#define INCLUDED_COM_DEBUG

#include "stddefx.h"



namespace com {
  // Debug declarations.
}



namespace com {

//! Prints the result of subtracting \a val1 from \a val2, and result.
/*!
  \param     val1 First value.
  \param     val2 Second value.
  \param     result Result of substraction.

  Information is printed on std::cout.
*/
template<class T>
void printCompDiff(const T& val1, const T& val2, const T& result) {
  std::cout << val1 - val2 << " == " << result << std::endl;
}



} // namespace com

#endif
