#ifndef INCLUDED_COM_UTIL
#define INCLUDED_COM_UTIL



#include <iostream>
#include <vector>
#include "csftypes.h"



/*!
  \struct com_Util
  \brief The com_Util class is for static utility functions.
*/
//       1         2         3         4         5         6         7         8
struct com_Util
{

  //! Find the smallest value out of {1, 2, 5}*10^n which is &gt;= x.1
  static double    ceil125             (double x);

  //! Fills a vector with linearly equally spaced values.
  static void      linSpace            (std::vector<double>::iterator begin,
                                        std::vector<double>::iterator end,
                                        double min,
                                        double max);

  //! Fills a vector with logarithmically equally spaced values.
  static void      logSpace            (std::vector<double>::iterator begin,
                                        std::vector<double>::iterator end,
                                        double min,
                                        double max);

  //! Checks if an array is a strictly monotonic sequence or not.
  static int       monotonic           (double *array, int size);

  //! Returns the scale version of \a value.
  static double    scale               (double value,
                                        double percentage);

};

namespace com {

  size_t           smallestDivisor     (size_t number,
                                        size_t start);

  size_t           largestDivisor      (size_t number,
                                        size_t start);

} // namespace com

#endif
